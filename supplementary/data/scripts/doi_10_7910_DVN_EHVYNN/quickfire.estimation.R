
###############################################
###############################################
###############################################
###############################################
###############################################


###### QUICKFIRE PAIRED COMPARISON ESTIMATION



###############################################
###############################################
###############################################
###############################################
###############################################


cat("Begin QUICKFIRE PAIRED COMPARISON ESTIMATION\n")


load("quickfire.data.final.Rdata")

pair.means.all<-quickfire.data.final
dim(pair.means.all)

##Item List and corresponding number in csv file (qualtrics data)
#1-"Commute Time"
#2-"Sidewalks"
#3-"Parking Capacity"
#4-"Amount Daily Driving"
#5-"School Qual."
#6-"Prop. Tax Rates"
#7-"Crime"
#8-"Gang Activity"
#9-"Drug Users"
#10-"Homeless"
#11-"Police Qual."
#12-"Judeo-Christian Nabes"
#13-"Dist. to Church"
#14-"Retiree Friendly"
#15-"Aetheist Friendly"
#16-"Amount Corruption"
#17-"Gay Friendly"
#18-"High-Dens. Housing"
#19-"Low-Dens. Housing"
#20-"Air Pollution"
#21-"Access Highways"
#22-"Access Bus/Rail"
#23-"Nabes Share Politics"
#24-"Bike Friendly"
#25-"Nabes Educated"
#26-"Walk to Shop"
#27-"Walk to Work/School"
#28-"Home Values"
#29-"Home Prices"
#30-"White Comm."
#31-"Black Comm."
#32-"Asian Comm."
#33-"Hisp. Comm."
#34-"Christian Comm."
#35-"Jewish Comm."
#36-"Muslim Comm."
#37-"Street Life"
#38-"Restaurants"
#39-"Privacy"
#40-"Nabes Share Relig. Vals."
#41-"Kid Friendly"
#42-"Number. Retirees"
#43-"Poor Comm."
#44-"Warm Climate"
#45-"Cold Climate"
#46-"Bus Child to School"
#47-"Business Friendly"
#48-"Gov. Employees"
#49-"Wealthy Comm."
#50-"Big Houses/Yards"
#51-"Friends Nearby"
#52-"Family Nearby"
#53-"Lot in Common w. Nabes"
#54-"Republican Comm."
#55-"Democratic Comm."
#56-"Local Sales Tax"
#57-"Low-Income Services"
#58-"Public Transit Qual."
#59-"Road Qual."
#60-"Parks Qual."
#61-"Prefer Not to Answer"
#62-"Region of Country"
#64-"Close to Maj. MSA"
#(total of 63 items, with #63 skipped in the data)

item.names<-c("Commute Time","Sidewalks","Parking Capacity","Amount Daily Driving","School Quality","Property Tax Rates","Crime","Gang Activity","Drug Users","Homeless","Police Quality","Judeo-Christian Neighbors","Distance to Church","Retiree Friendly","Atheist Friendly","Amount of Corruption","Gay Friendly","High-Density Housing","Low-Density Housing","Air Pollution","Access to Highways","Access to Bus/Rail","Neighbors Share Politics","Bike Friendly","Neighbors Educated","Walk to Shopping","Walk to Work/School","Home Values","Home Prices","White Community","Black Community","Asian Community","Hispanic Community","Christian Community","Jewish Community","Muslim Community","Street Life","Restaurants","Privacy","Neighbors Share Religious Values","Kid Friendly","Number of Retirees","Poor Community","Warm Climate","Cold Climate","Bus Child to School","Business Friendly","Government Employees","Wealthy Community","Big Houses/Yards","Friends Nearby","Family Nearby","Lot in Common w/ Neighbors","Republican Community","Democratic Community","Local Sales Tax","Low-Income Services","Public Transit Quality","Road Quality","Parks Quality","Prefer Not to Answer","Region of Country","Close to Major Metro Area")



#####
#### CALCULATE MEAN SUPPORT FOR EACH ITEM CONDITIONAL ON HAVING SEEN IT
#####
#####
##all respondents, dems and reps


pair.means.all<-quickfire.data.final
dim(pair.means.all)
pair.means.dems<-subset(pair.means.all, pair.means.all$partywlean=="dem")
dim(pair.means.dems)	

pair.means.reps<-subset(pair.means.all, pair.means.all$partywlean=="rep")

pair.means.data<-list(pair.means.all, pair.means.dems, pair.means.reps)

##make empty data frames to store results	
means<-NA
lb<-NA
ub<-NA
item<-c(seq(1,62,by=1), 64)
label<-rep(3,times=63)##all label = the number 3

all.means<-cbind.data.frame(party.label=label,item.index=item,item.names=item.names,means=means,lb=lb,ub=ub)


label<-rep(1,times=63)##dem label = the number 1
dem.means<-cbind.data.frame(party.label=label,item.index=item,item.names=item.names,means=means,lb=lb,ub=ub)


label<-rep(2,times=63)##rep label = the number 2
rep.means<-cbind.data.frame(party.label=label,item.index=item,item.names=item.names,means=means,lb=lb,ub=ub)

quick.means.list<-list(all.means=all.means, dem.means=dem.means, rep.means=rep.means)

##get means and CIs for all respondents, dems and reps



for (j in 1:3){
for (i in 1:63){
clean.data<-(pair.means.data[[j]][,i])
clean.data<-na.omit(clean.data)
##estimate the means and 95% CIs for the three samples
a<-t.test(clean.data, na.rm=TRUE)
quick.means.list[[j]][i, "means"]<-as.n(a$estimate)
quick.means.list[[j]][i,"lb"]<-as.n(a$conf.int[1])
quick.means.list[[j]][i,"ub"]<-as.n(a$conf.int[2])
}
}

names(quick.means.list)

rep.means<-quick.means.list$rep.means[order(quick.means.list$rep.means$means ,   decreasing = TRUE ) , ]
rep.means$sort<-c(len(rep.means$means):1)
rep.order<-cbind.data.frame(sort=rep.means$sort, item.names=rep.means$item.names)
dem.means <- merge(quick.means.list$dem.means,rep.order,by="item.names", all.x =TRUE)##merge in order
dem.means
all.means <- merge(quick.means.list$all.means,rep.order,by="item.names", all.x =TRUE)##merge in GOP sort variable
all.means

quick.means<-rbind.data.frame(all.means, dem.means,rep.means)
quick.cat<-read.csv("ssi--pairedcompcategories.csv") ##read in category labels



quick.means <- merge(quick.means,quick.cat,by="item.index", all.x =TRUE)##merge in category labels
dim(quick.means)
subcat<-as.c(unique(quick.means$subcat))##create vector of categories





##find out rank order of categories by mean level of response in the all.means data
quick.means.all<-subset(quick.means, party.label==3)##grab subset of data for "All" results
quick.means.all
mean.cat<-NA
cat.means<-cbind.data.frame(subcat, mean.cat)
cat.means[,1]<-as.c(cat.means[,1])
quick.means.all$subcat<-as.c(quick.means.all$subcat)
quick.means.all$means<-as.n(as.c(quick.means.all$means))
cat.means[,2]<-as.n(cat.means[,2])

cat.means[1,2]<-mean.transpo<-mean(quick.means.all$means[quick.means.all$subcat=="Transportation"] )
cat.means[2,2]<-mean.blight<-mean(quick.means.all$means[quick.means.all$subcat=="Smart Growth v. Sprawl"] )
cat.means[3,2]<-mean.gov<-mean(quick.means.all$means[quick.means.all$subcat=="Children"] )
cat.means[4,2]<-mean.vals<-mean(quick.means.all$means[quick.means.all$subcat=="Government"])
cat.means[5,2]<-mean.soc<-mean(quick.means.all$means[quick.means.all$subcat=="Blight & Crime"] )
cat.means[6,2]<-mean.grow<-mean(quick.means.all$means[quick.means.all$subcat=="Neighborhood Beliefs/Values"] )
cat.means[7,2]<-mean.income<-mean(quick.means.all$means[quick.means.all$subcat=="Social Life"] )
cat.means[8,2]<-mean.race<-mean(quick.means.all$means[quick.means.all$subcat=="Neighborhood Income"] )
cat.means[9,2]<-mean.kids<-mean(quick.means.all$means[quick.means.all$subcat=="Neighborhood Race"] )
cat.means[10,2]<-mean.geo<-mean(quick.means.all$means[quick.means.all$subcat=="Geography/Location"] )
cat.means[11,2]<-mean.fam<-mean(quick.means.all$means[quick.means.all$subcat=="Friends & Family"] )
cat.means

quick.means <- merge(quick.means,cat.means,by="subcat", all.x =TRUE)


##order the data for graphing, first by the level of the group mean, then by question, then by mean
sort.quick.means.graph<-subset(quick.means,  quick.means$party.label!=3)##eliminate all.means data
dim(sort.quick.means.graph)
sort.quick.means.graph<-sort.quick.means.graph[order(sort.quick.means.graph$mean.cat, sort.quick.means.graph$sort, sort.quick.means.graph$party.label ,  decreasing = TRUE ) , ]
sort.quick.means.graph[1:20,]

##make another sorting variable so other graphs can follow the same order (for comparability)
sort.quick.means.graph$sort.num<-rep(1:len(sort.quick.means.graph$means))
sort.quick.means.graph$ID<-paste(sort.quick.means.graph$party.label,sort.quick.means.graph$cat,sep="-" )
sort.quick.means.graph


##extract that and item.index
master.quick.order<-cbind.data.frame(ID=sort.quick.means.graph$ID, sort.num=sort.quick.means.graph$sort.num)
head(master.quick.order)

file.name<-"master.quick.order.Rdata"
save(master.quick.order, file=paste(output,file.name,sep=""))





##save results

file.name="sort.quick.means.graph.parties.csv"
write.table(sort.quick.means.graph,file=paste(output,file.name,sep=""),sep=",",row.names=T)


file.name="sort.quick.means.graph.Rdata"
save(sort.quick.means.graph, file=paste(output,file.name,sep="") )


head(sort.quick.means.graph)






##### GRAPH MAIN PAIRED COMPARISON RESULTS

sort.quick.means.graph$subcat<-as.c(sort.quick.means.graph$subcat)
var.names<-as.c(unique(sort.quick.means.graph$item.names))
var.names##now manually add spaces in between each 
var.names<-c("Crime", "","Gang Activity", "", "Drug Users", "","Air Pollution", "",
 "Homeless", "","Region of Country", "", "Warm Climate", "","Cold Climate", "", 
 "Close to Major Metro Area", "","Family Nearby", "", "Friends Nearby", "","Home Prices", "",
 "Home Values", "", "Poor Community", "","Wealthy Community", "", "Property Tax Rates", "", 
 "Police Quality", "","Parks Quality", "", "Local Sales Tax", "", "Business Friendly", "",
 "Amount of Corruption", "","Low-Income Services", "", "Government Employees", "","Amount Daily Driving", "", 
 "Road Quality", "","Commute Time", "","Access to Highways", "","Bike Friendly", "",
 "Public Transit Quality", "","Access to Bus/Rail", "","Privacy", "", "Big Houses/Yards", "", 
 "Low-Density Housing", "", "High-Density Housing", "","Walk to Work/School", "", "Walk to Shopping", "", 
 "Parking Capacity", "","Sidewalks", "", "School Quality", "","Kid Friendly", "", 
"Bus Child to School", "", "Restaurants", "", "Retiree Friendly", "","Number of Retirees", "", 
 "Street Life", "", "Lot in Common w/ Neighbors", "","Christian Community", "", "Distance to Church", "", 
 "Neighbors Educated", "","Neighbors Share Religious Values", "","Judeo-Christian Neighbors", "", "Republican Community", "", "Neighbors Share Politics", "","Muslim Community", "","Atheist Friendly", "","Democratic Community", "", "Gay Friendly", "","Jewish Community", "","White Community", "", "Black Community", "", "Hispanic Community", "","Asian Community", "", "Prefer Not to Answer", "") 
len(var.names)


##label dems and reps
sort.quick.means.graph$party<-NA
sort.quick.means.graph$party[sort.quick.means.graph$party.label==1
]<-"Dem"
sort.quick.means.graph$party[sort.quick.means.graph$party.label==2
]<-"Rep"

##make diff shapes for dems and reps
sort.quick.means.graph$points<-NA
sort.quick.means.graph$points[sort.quick.means.graph$party=="Rep"]<-19
sort.quick.means.graph$points[sort.quick.means.graph$party=="Dem"]<-17


coef.vec <- as.n(as.character( sort.quick.means.graph$means))
lb.vec <- as.n(as.character( sort.quick.means.graph$lb))
ub.vec <- as.n(as.character(sort.quick.means.graph$ub))
y.axis <- c(length(coef.vec):1)

len(coef.vec)==len(lb.vec)
len(lb.vec)==len(ub.vec)
len(coef.vec)==len(y.axis)

ticks<-seq(max(y.axis)-.5, min(y.axis)+.5, -2)


file.name<-"quickfire.means.pdf"

##GENERATE FIGURE 3 in manuscript

pdf(paste(output,file.name,sep=""), paper='special', height=8, width=7)
par(mar=c(4, 10, 2.5, 8))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Proportion Listing Trait as Important 
over Randomly Paired Alternatives", ylab = "",cex = .4,
xlim = c(0,1), xaxs = "i", main = "Partisans Agree on Community Traits Ranked Most Important, 
Disagree On Low-Importance Social Indicators", col=c("red","blue"), pch = sort.quick.means.graph$points) 
segments(lb.vec, y.axis, ub.vec, y.axis, lwd =  .6, col=c("red","blue"))
axis(1, at = seq(0,1,by=.1), labels =  c(0,seq(.1,.9,by=.1),1), tick = T, tck= -.02, cex.axis = .8, mgp = c(2,.7,0)) 
axis(2, at=c(-4,y.axis), label = FALSE, las = 1, tick = T, 
mgp = c(2,1,0),tck=0, cex.axis = .7) 
axis(2, at=ticks, label = FALSE, las = 1, tick = T, 
mgp = c(2,1,0),tck=-.01, cex.axis = .7) 
axis(4, at = c(-4,y.axis), label = FALSE, las = 1, tick = T, 
mgp = c(2,1,0),tck=0, cex.axis = .7) 
abline(h=seq(2.5,124.5,by=2),lwd=.01, col="gray")
abline(h=c(114.5,106.5,102.5,94.5,78.5,64.5,48.5,42.5,34.5,8.5, .5)+2, lty=1, lwd=1, col="black")
##add internal ticks
segments(x0=rep(seq(-1,1,by=.1), 11) ,y0=c(116.5, 108.5, 104.5, 96.5, 80.5, 66.5, 50.5, 44.5, 36.5, 10.5, 2.5), x1=rep(seq(-1,1,by=.1), 11),y1=c(116.5, 108.5, 104.5, 96.5, 80.5, 66.5, 50.5, 44.5, 36.5, 10.5, 2.5)-.25, col="black")
mtext(var.names, side=2, at=seq(125.5,.5,by=-1),las=2,cex=.63, line=.5)
mtext(c("Disorder", "Geography/Location", "Friends & Family","Neighborhood 
Income","Government", "Transportation","Smart Growth 
v. Sprawl", "Children","Social Life", "Neighborhood 
Beliefs/Values", "Neighborhood 
Race") , side=4, at=c(118.5, 110.5, 104.5, 98.5, 86.5, 70.5, 55.5, 45.5, 38.5, 22.5, 4.5)+2, las=2, line=.2, cex=1)

legend("bottomright", legend=c("Republicans","Democrats"), cex=.45, col=c("red","blue"), pch=sort.quick.means.graph$points)

dev.off()







#######
##### PAIRED COMPARISON: SORTED v. UNSORTED PARTISANS
########
#######


##first just sorted v. unsorted partisans
subset.data.libdem<-list(NA)
subset.data.otherdem<-list(NA)
subset.data.conrep<-list(NA)
subset.data.otherrep<-list(NA)

##isolate sorted and unsorted partisans
subset.data.libdem<-subset(pair.means.all, pair.means.all$partywlean=="dem" & pair.means.all$libdem==1)
subset.data.otherdem<-subset(pair.means.all, pair.means.all$partywlean=="dem" & pair.means.all$libdem==0)
subset.data.conrep<-subset(pair.means.all, pair.means.all$partywlean=="rep" & pair.means.all$conrep==1)
subset.data.otherrep<-subset(pair.means.all, pair.means.all$partywlean=="rep" & pair.means.all$conrep==0)

sorted.data<-list(libdem.data=subset.data.libdem, conrep.data=subset.data.conrep, otherdem.data=subset.data.otherdem, otherrep.data=subset.data.otherrep)

##make a plce to store results
subset.results.libdem<-list(NA)
subset.results.otherdem<-list(NA)
subset.results.conrep<-list(NA)
subset.results.otherrep<-list(NA)

subset.results.libcon<-list(subset.results.libdem=subset.results.libdem, subset.results.conrep=subset.results.conrep, subset.results.otherdem=subset.results.otherdem, subset.results.otherrep=subset.results.otherrep)

for (j in 1:4){
mat<-as.data.frame(matrix(NA, nrow=63, ncol=3))

if (j%in%1:2){## lib dems and con reps
mat$item.index<-c(seq(1,62,by=1), 64)
mat$item.names<-item.names
mat$label<-rep(2,times=63)##sorted gets a 2
names(mat)<-c("means", "lb","ub", "item.index", "item.names", "label")
subset.results.libcon[[j]]<-mat
subset.results.libcon[[j]]$party.label<-"sorted"}

if (j%in%3:4){##other dems and other reps
mat$item.index<-c(seq(1,62,by=1), 64)
mat$item.names<-item.names
mat$label<-rep(1,times=63)##unsorted gets a 1
names(mat)<-c("means", "lb","ub", "item.index", "item.names", "label")
subset.results.libcon[[j]]<-mat
subset.results.libcon[[j]]$party.label<-"unsorted"}
}
head(subset.results.libcon[[1]])


##estimate the means for all liberal and other democrats/con and other republicans
for (j in 1:4){
for (i in 1:63){
	a<-t.test(sorted.data[[j]][,i], na.rm=TRUE)
	subset.results.libcon[[j]]$means[i]<-as.n(a$estimate)
	subset.results.libcon[[j]]$lb[i]<-as.n(a$conf.int[1])
	subset.results.libcon[[j]]$ub[i]<-as.n(a$conf.int[2])
	}
}

dem.means.libdem<-rbind.data.frame(subset.results.libcon$subset.results.libdem, subset.results.libcon$subset.results.otherdem)
rep.means.conrep<-rbind.data.frame(subset.results.libcon$subset.results.conrep, subset.results.libcon$subset.results.otherrep)

quick.cat<-read.csv("ssi--pairedcompcategories.csv") ##read in category labels
dem.means.libdem <- merge(dem.means.libdem,quick.cat,by="item.index", all.x =TRUE)##merge in category labels

dem.means.libdem$ID<-paste(dem.means.libdem$label,dem.means.libdem$cat,sep="-" )

file.name<-"master.quick.order.Rdata"
load(paste(output,file.name,sep=""))


dem.means.libdem <- merge(dem.means.libdem,master.quick.order,by="ID", all.x =TRUE)
dem.means.libdem$label<-as.n(dem.means.libdem$label)
dem.means.libdem.graph<-dem.means.libdem[order(dem.means.libdem$sort.num, dem.means.libdem$label ) , ]
dem.means.libdem.graph[1:20,]

write.table(dem.means.libdem.graph, file=paste(output,"quickfire.libdem.graph.csv",sep=""))




rep.means.conrep <- merge(rep.means.conrep,quick.cat,by="item.index", all.x =TRUE)##merge in category labels
rep.means.conrep$ID<-paste(rep.means.conrep$label,rep.means.conrep$cat,sep="-" )
rep.means.conrep <- merge(rep.means.conrep,master.quick.order,by="ID", all.x =TRUE)
rep.means.conrep$label<-as.n(rep.means.conrep$label)
rep.means.conrep.graph<-rep.means.conrep[order(rep.means.conrep$sort.num, rep.means.conrep$label ) , ]
rep.means.conrep.graph[1:20,]

file.name<-paste(output,"quickfire.conrep.graph.csv",sep="")

write.table(rep.means.conrep.graph,file=file.name,sep=",",row.names=T)

sorted.partisans.graph.datalist<-list(dem.means.libdem.graph, rep.means.conrep.graph)

names(sorted.partisans.graph.datalist)<-c("dem.means.libdem.graph", "rep.means.conrep.graph")

file.name<-paste(output,"sorted.partisans.graph.datalist.Rdata",sep="")

save(sorted.partisans.graph.datalist, file=file.name )



filenames<-c("quickfire.libdem","quickfire.conrep")

var.names<-c("Crime", "","Gang Activity", "", "Drug Users", "","Air Pollution", "",
 "Homeless", "","Region of Country", "", "Warm Climate", "","Cold Climate", "", 
 "Close to Major Metro Area", "","Family Nearby", "", "Friends Nearby", "","Home Prices", "",
 "Home Values", "", "Poor Community", "","Wealthy Community", "", "Property Tax Rates", "", 
 "Police Quality", "","Parks Quality", "", "Local Sales Tax", "", "Business Friendly", "",
 "Amount of Corruption", "","Low-Income Services", "", "Government Employees", "","Amount Daily Driving", "", 
 "Road Quality", "","Commute Time", "","Access to Highways", "","Bike Friendly", "",
 "Public Transit Quality", "","Access to Bus/Rail", "","Privacy", "", "Big Houses/Yards", "", 
 "Low-Density Housing", "", "High-Density Housing", "","Walk to Work/School", "", "Walk to Shopping", "", 
 "Parking Capacity", "","Sidewalks", "", "School Quality", "","Kid Friendly", "", 
"Bus Child to School", "", "Restaurants", "", "Retiree Friendly", "","Number of Retirees", "", 
 "Street Life", "", "Lot in Common w/ Neighbors", "","Christian Community", "", "Distance to Church", "", 
 "Neighbors Educated", "","Neighbors Share Religious Values", "","Judeo-Christian Neighbors", "", "Republican Community", "", 
 "Neighbors Share Politics", "","Muslim Community", "","Atheist Friendly", "","Democratic Community", "", 
 "Gay Friendly", "","Jewish Community", "","White Community", "", "Black Community", "", "Hispanic Community", "","Asian Community", "", "Prefer Not to Answer", "") 




for(i in 1:2){
	
coef.vec <- as.n(as.character( sorted.partisans.graph.datalist[[i]]$means))
lb.vec <- as.n(as.character( sorted.partisans.graph.datalist[[i]]$lb))
ub.vec <- as.n(as.character(sorted.partisans.graph.datalist[[i]]$ub))
y.axis <- c(length(coef.vec):1)
ticks<-seq(max(y.axis)-.5, min(y.axis)+.5, -2)
file<-paste(output, filenames[i], ".means.pdf",sep="")
if(i==1){main.title<-"Paired Comparison Results:
	Sorted v. Unsorted Democrats"
colors<-c("blue","lightskyblue")
leg<-c("Liberal Dem.","Other Dem.")}
if (i==2){main.title<-"Paired Comparison Results:
	Sorted v. Unsorted Republicans"
colors<-c("red","lightpink1")
leg<-c("Conservative Rep.","Other Rep.")}
pdf(file,  paper='special', height=8, width=7)
par(mar=c(4, 10, 2.5, 8))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Proportion Listing Trait as Important 
over Randomly Paired Alternatives", ylab = "", pch = 19, cex = .4, xlim = c(0,1), xaxs = "r", main = main.title, col=colors) 
segments(lb.vec, y.axis, ub.vec, y.axis, lwd =  .6, col=colors)
axis(1, at = seq(-.1,1,by=.1), labels =  T, tick = T, tck= -.01, cex.axis = .8, mgp = c(2,.7,0)) 
axis(2, at=c(-4,y.axis), label = FALSE, las = 1, tick = T, 
mgp = c(2,1,0),tck=0, cex.axis = .7) 
axis(2, at=ticks, label = FALSE, las = 1, tick = T, 
mgp = c(2,1,0),tck=-.01, cex.axis = .7) 
axis(4, at = c(-4,y.axis), label = FALSE, las = 1, tick = T, 
mgp = c(2,1,0),tck=0, cex.axis = .7) 
abline(h=seq(2.5,124.5,by=2),lwd=.01, col="gray")
abline(h=c(114.5,106.5,102.5,94.5,78.5,64.5,48.5,42.5,34.5,8.5, .5)+2, lty=1, lwd=1, col="black")
##add internal ticks
##add internal ticks
segments(x0=rep(seq(-1,1,by=.1), 11) ,y0=c(116.5, 108.5, 104.5, 96.5, 80.5, 66.5, 50.5, 44.5, 36.5, 10.5, 2.5), x1=rep(seq(-1,1,by=.1), 11),y1=c(116.5, 108.5, 104.5, 96.5, 80.5, 66.5, 50.5, 44.5, 36.5, 10.5, 2.5)-.25, col="black")
mtext(var.names, side=2, at=seq(125.5,.5,by=-1),las=2,cex=.63, line=.5)
mtext(c("Disorder", "Geography/Location", "Friends & Family","Neighborhood 
Income","Government", "Transportation","Smart Growth 
v. Sprawl", "Children","Social Life", "Neighborhood 
Beliefs/Values", "Neighborhood 
Race") , side=4, at=c(118.5, 110.5, 104.5, 98.5, 86.5, 70.5, 55.5, 45.5, 38.5, 22.5, 4.5)+2,line=.2, las=2, cex=1)
legend("bottomright", leg, cex=0.4, pch=19,  col=colors)



dev.off()
}



##### PAIRED COMPARISON SUBSETS



for (i in 1:ncol(pair.means.all)){
	if (colnames(pair.means.all)[i]=="white"){colnames(pair.means.all)[i]<-"white_race"}
	if (colnames(pair.means.all)[i]=="educ"){colnames(pair.means.all)[i]<-"educ2"}
	
	}
names(pair.means.all)

####
####
###Dems v. Reps on SUBSETS
####
####
#now all the subsets

sort.names<-c("white_race","female","rich","old","ownrent","educ2","homelast5","kidshome","city","black.therm","black.hisp.therm","white.black.diff","young1", "young2","young3","single","working","retired","same.msa")

##make the subset data sets for both parties =1 on each subset var; =0 on each subset var

##isolate the dems and reps
dem.data<-subset(pair.means.all, pair.means.all$partywlean=="dem")
rep.data<-subset(pair.means.all, pair.means.all$partywlean=="rep")
dim(dem.data)
data.party<-list(dem.data, rep.data)

subset.data.dem.1<-list(NA)
subset.data.dem.0<-list(NA)
subset.data.rep.1<-list(NA)
subset.data.rep.0<-list(NA)

#####CREATE SUBSETS
for (j in 1:2){###dem or rep data set?
for (k in 0:1){###does trait==1 or 0?
for (i in 1:len(sort.names)){##which subset?
	
	if (j==1 & k==1){###dems with trait==1
	subset.data.dem.1[[i]]<-subset(data.party[[j]],data.party[[j]][,grep(sort.names[i], names(data.party[[j]]), fixed=TRUE)]==k) }
	if (j==1 & k==0){###dems with trait==0
	subset.data.dem.0[[i]]<-subset(data.party[[j]],data.party[[j]][,grep(sort.names[i], names(data.party[[j]]), fixed=TRUE)]==k)}
	if (j==2 & k==1){##reps with trait==1
	subset.data.rep.1[[i]]<-subset(data.party[[j]],data.party[[j]][,grep(sort.names[i], names(data.party[[j]]), fixed=TRUE)]==k)}
	if (j==2 & k==0){##reps with trait==0
	subset.data.rep.0[[i]]<-subset(data.party[[j]],data.party[[j]][,grep(sort.names[i], names(data.party[[j]]), fixed=TRUE)]==k)}
}
}
}

##give an intuitive name to each subset of the data
names(subset.data.dem.1)<-paste("dem.", sort.names,".1", sep="")
names(subset.data.dem.0)<-paste("dem.", sort.names,".0", sep="")
names(subset.data.rep.1)<-paste("rep.", sort.names,".1", sep="")
names(subset.data.rep.0)<-paste("rep.", sort.names,".0", sep="")

##run some checks
table(dem.data$old)[2]==dim(subset.data.dem.1$dem.old.1)[1]
table(rep.data$kidshome)[1]==dim(subset.data.rep.0$rep.kidshome.0)[1]
table(rep.data$educ2)[1]==dim(subset.data.rep.0$rep.educ2.0)[1]
##these all check out


##now get all the means
subset.results.dem.1<-list(NA)
subset.results.dem.0<-list(NA)
subset.results.rep.1<-list(NA)
subset.results.rep.0<-list(NA)

##make empty data frames to store results
for (j in 1:2){
for (i in 1:len(sort.names)){
mat<-as.data.frame(matrix(NA, nrow=63, ncol=3))
if (j==1){
mat$item.index<-c(seq(1,62,by=1), 64)
mat$item.names<-item.names
mat$label<-rep(1,times=63)##dem gets a 1
names(mat)<-c("means", "lb","ub", "item.index", "item.names", "label")
subset.results.dem.1[[i]]<-mat
subset.results.dem.1[[i]]$party.label<-"Democrat"
subset.results.dem.0[[i]]<-mat
subset.results.dem.0[[i]]$party.label<-"Democrat"}

if (j==2){
mat$item.index<-c(seq(1,62,by=1), 64)
mat$item.names<-item.names
mat$label<-rep(2,times=63)##rep gets a 2
names(mat)<-c("means", "lb","ub", "item.index", "item.names", "label")
subset.results.rep.1[[i]]<-mat
subset.results.rep.1[[i]]$party.label<-"Republican"
subset.results.rep.0[[i]]<-mat
subset.results.rep.0[[i]]$party.label<-"Republican"}
}
}

##check format
head(subset.results.dem.1[[1]])
head(subset.results.dem.0[[1]])
head(subset.results.rep.1[[5]])
head(subset.results.rep.0[[5]])


##combine all empty data frames into a single list and give each data frame an intuitive name
subset.results.all<-list(subset.results.dem.1=subset.results.dem.1, subset.results.dem.0=subset.results.dem.0, subset.results.rep.1=subset.results.rep.1, subset.results.rep.0=subset.results.rep.0)

for (i in 1:4){
	names(subset.results.all[[i]])<-sort.names
	}

##check names
names(subset.results.all )
names(subset.results.all[[1]])

party.data.all<-list(subset.data.dem.1,subset.data.dem.0,subset.data.rep.1,subset.data.rep.0)

##estimate the means for all subsets
for (j in 1:4){
for (k in 1:len(sort.names)){
for (i in 1:63){
	a<-t.test(party.data.all[[j]][[k]][,i], na.rm=TRUE)
	subset.results.all[[j]][[k]]$means[i]<-as.n(a$estimate)
	subset.results.all[[j]][[k]]$lb[i]<-as.n(a$conf.int[1])
	subset.results.all[[j]][[k]]$ub[i]<-as.n(a$conf.int[2])
	}
}
}

##the following syntax can now access a specific set of results
subset.results.all$subset.results.dem.1$white_race
subset.results.all$subset.results.dem.1$young3
head(subset.results.all$subset.results.rep.1$educ)
head(subset.results.all$subset.results.dem.0$educ)

quickfire.subset.results<-subset.results.all



####rbind all the analgous sorted and unsorted data frames
##for those where subset var=1, dems and reps
trait.1<-list(NA)
trait.0<-list(NA)
for (i in 1:len(sort.names)){
	trait.1[[i]]<-rbind.data.frame(subset.results.all$subset.results.dem.1[[i]], subset.results.all$subset.results.rep.1[[i]] )
	}
##for those where subset var=0, dems and reps
trait.0<-list(NA)
for (i in 1:len(sort.names)){
	trait.0[[i]]<-rbind.data.frame(subset.results.all$subset.results.dem.0[[i]], subset.results.all$subset.results.rep.0[[i]] )
	}

##check this
trait.1[[1]]

subset.data.full<-list(trait.1=trait.1, trait.0=trait.0 )

##sort all the dataframes so they are ready for graphing
for (j in 1:2){
for (i in 1:len(sort.names)){
subset.data.full[[j]][[i]] <- merge(subset.data.full[[j]][[i]],quick.cat,by="item.index", all.x =TRUE)##merge in category labels
subset.data.full[[j]][[i]]$ID<-paste(subset.data.full[[j]][[i]]$label,subset.data.full[[j]][[i]]$cat,sep="-" )
subset.data.full[[j]][[i]] <- merge(subset.data.full[[j]][[i]],master.quick.order,by="ID", all.x =TRUE)
subset.data.full[[j]][[i]]$label<-as.n(subset.data.full[[j]][[i]]$label)
subset.data.full[[j]][[i]]<-subset.data.full[[j]][[i]][order(subset.data.full[[j]][[i]]$sort.num, subset.data.full[[j]][[i]]$label ) , ]
}}




##replace "lb and "ub" "NaNs" with 0s (these are instances where the mean is 0 exactly)
for (j in 1:2){
for (k in 1:len(subset.data.full[[1]])){
for (h in 1:length(c("lb","ub"))){
for (i in 1:len(subset.data.full[[1]][[1]][,1])){	
	
	if (is.nan(subset.data.full[[j]][[k]][i,c("lb","ub")[h]])==TRUE){
	subset.data.full[[j]][[k]][i,c("lb","ub")[h]]<-0}
	
	
	}}}}




##give each data frame of results an intuitive name
names(subset.data.full[[1]])<-sort.names
names(subset.data.full[[2]])<-sort.names

##check this
head(subset.data.full[[2]][[8]])




###the object subset.data.full now contains all the subset quickfire results, sorted for graphing
file.name<-paste(output,"quickfire.subset.data.full.Rdata",sep="")
save(subset.data.full, file=file.name)



sort.names<-c("white_race","female","rich","old","ownrent","educ2","homelast5","kidshome","city","black.therm","black.hisp.therm","white.black.diff","young1", "young2","young3","single","working","retired","same.msa")


##make the graphs with a loop
filenames<-list(paste(sort.names, ".1", sep=""), paste(sort.names, ".0", sep=""))
title.trait<-list(c("Whites","Females","Income > $80k/Year","Over 65","Homeowners","Educated","Moved in Last 5 Years", "Those w/ Kids","City Residents","> Median on Black Feeling Therm.","> Median on Black/Hispanic Feeling Therm.","> Median White-Black Feel. Therm Difference", "18-25 Yrs. Old", "18-30 Yrs. Old", "18-35 Yrs. Old", "Single","Working","Retired","Moved to Same MSA"), 
c( "Nonwhites" , "Males", "Income <= $80k/Year","<= 65","Renters","Non-Educated","Not Moved in Last 5 Years", "Those w/out Kids","Non-city Residents","<= Median on Black Feeling Therm.","<= Median on Black/Hispanic Feeling Therm.","<= Median White-Black Feel. Therm Difference", "Not 18-25 Yrs. Old", "Not 18-30 Yrs. Old", "Not 18-35 Yrs. Old","Not Single","Not Working","Not Retired","Did Not Move to Same MSA"))
colors<-c("red", "blue")

##make diff shapes for dems and reps
for(j in 1:2){##trait = 1 or trait = 0
for(i in 1:len(sort.names) ){ ##which trait?
subset.data.full[[j]][[i]]$points<-NA
subset.data.full[[j]][[i]]$points[subset.data.full[[j]][[i]]$party.label=="Republican"]<-19
subset.data.full[[j]][[i]]$points[subset.data.full[[j]][[i]]$party.label=="Democrat"]<-17
}}



for(j in 1:2){##trait = 1 or trait = 0
for(i in 1:len(sort.names) ){ ##which trait?
coef.vec <- as.n(as.character( subset.data.full[[j]][[i]]$means))
lb.vec <- as.n(as.character( subset.data.full[[j]][[i]]$lb))
ub.vec <- as.n(as.character(subset.data.full[[j]][[i]]$ub))
y.axis <- c(length(coef.vec):1)

ticks<-seq(max(y.axis)-.5, min(y.axis)+.5, -2)

file<-paste(output,"quickfire.", filenames[[j]][i], ".means.pdf",sep="")
pdf(file,  paper='special', height=8, width=7)
par(mar=c(4, 10, 2.5, 8))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Proportion Listing Trait as Important 
over Randomly Paired Alternatives", ylab = "", pch = subset.data.full[[j]][[i]]$points, cex = .4, xlim = c(0,1), xaxs = "i", main=(paste("Paired Comparison Results:
",title.trait[[j]][i], " Only", sep="")), col=colors) 
segments(lb.vec, y.axis, ub.vec, y.axis, lwd =  .6, col=colors)
axis(1, at = seq(0,1,by=.1), label=T, tick = T, tck= -.01, cex.axis = .8, mgp = c(2,.7,0)) 
axis(2, at=c(-4,y.axis), label = FALSE, las = 1, tick = T, 
mgp = c(2,1,0),tck=0, cex.axis = .7) 
axis(2, at=ticks, label = FALSE, las = 1, tick = T, 
mgp = c(2,1,0),tck=-.01, cex.axis = .7) 
axis(4, at = c(-4,y.axis), label = FALSE, las = 1, tick = T, 
mgp = c(2,1,0),tck=0, cex.axis = .7) 
abline(h=seq(2.5,124.5,by=2),lwd=.01, col="gray")
abline(h=c(114.5,106.5,102.5,94.5,78.5,64.5,48.5,42.5,34.5,8.5, .5)+2, lty=1, lwd=1, col="black")
mtext(var.names, side=2, at=seq(125.5,.5,by=-1),las=2,cex=.63, line=.5)
##add internal ticks
##add internal ticks
segments(x0=rep(seq(-1,1,by=.1), 11) ,y0=c(116.5, 108.5, 104.5, 96.5, 80.5, 66.5, 50.5, 44.5, 36.5, 10.5, 2.5), x1=rep(seq(-1,1,by=.1), 11),y1=c(116.5, 108.5, 104.5, 96.5, 80.5, 66.5, 50.5, 44.5, 36.5, 10.5, 2.5)-.25, col="black")
#mtext(var.names, side=2, at=seq(125.5,.5,by=-1),las=2,cex=.5, line=.5)
mtext(c("Disorder", "Geography/Location", "Friends & Family","Neighborhood 
Income","Government", "Transportation","Smart Growth 
v. Sprawl", "Children","Social Life", "Neighborhood 
Beliefs/Values", "Neighborhood 
Race") , side=4, at=c(118.5, 110.5, 104.5, 98.5, 86.5, 70.5, 55.5, 45.5, 38.5, 22.5, 4.5)+2, las=2,line=.2, cex=1)

legend("bottomright", legend=c("Republicans","Democrats"), cex=.45, col=c("red","blue"), pch=sort.quick.means.graph$points)

dev.off()
}}


cat("End QUICKFIRE PAIRED COMPARISON ESTIMATION\n")


