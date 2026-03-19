





#################################
#####
##### CONJOINT ANALYSIS, CORE RESULTS
#####
#####
#################################

##CLUSTER ROBUST STANDARD ERRORS
###function for calculating cluster robust SEs (Arai 2011)
clx <- 
function(fm, dfcw, cluster){
         # R-codes (www.r-project.org) for computing
         # clustered-standard errors. Mahmood Arai, Jan 26, 2008.
	 # The arguments of the function are:
         # fitted model, cluster1 and cluster2
  # You need to install libraries `sandwich' and `lmtest'
	 
   # reweighting the var-cov matrix for the within model
         library(sandwich);library(lmtest)
         M <- length(unique(cluster))   
         N <- length(cluster)           
         K <- fm$rank                        
         dfc <- (M/(M-1))*((N-1)/(N-K))  
         uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
         vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)*dfcw
         coeftest(fm, vcovCL) }

load("conj.data.party.Rdata")
dim(conj.data.party)

trait.cols<-grep("conj", names(conj.data.party))

##recode as factor variables for estimation

table(conj.data.party$conj.house)
conj.data.party$conj.house<-factor(conj.data.party$conj.house)
class(conj.data.party$conj.house)
levels(conj.data.party$conj.house)


table(conj.data.party$conj.pres)
conj.data.party$conj.pres<-factor(conj.data.party$conj.pres, levels=c("50% Democrat, 50% Republican ", "30% Democrat, 70% Republican ", "70% Democrat, 30% Republican "))
class(conj.data.party$conj.pres)
levels(conj.data.party$conj.pres)

table(conj.data.party$conj.race)
conj.data.party$conj.race<-factor(conj.data.party$conj.race)
class(conj.data.party$conj.race)
levels(conj.data.party$conj.race)

table(conj.data.party$conj.school)
conj.data.party$conj.school<-factor(conj.data.party$conj.school)
class(conj.data.party$conj.school)
levels(conj.data.party$conj.school)

table(conj.data.party$conj.commute)
conj.data.party$conj.commute<-factor(conj.data.party$conj.commute)
class(conj.data.party$conj.commute)
levels(conj.data.party$conj.commute)

table(conj.data.party$conj.crime)
conj.data.party$conj.crime<-factor(conj.data.party$conj.crime)
class(conj.data.party$conj.crime)
levels(conj.data.party$conj.crime)

table(conj.data.party$conj.place)
conj.data.party$conj.place<-factor(conj.data.party$conj.place)
class(conj.data.party$conj.place)
levels(conj.data.party$conj.place)


##subset by party
conj.data.dems<-subset(conj.data.party, conj.data.party$partywlean=="dem")
conj.data.reps<-subset(conj.data.party, conj.data.party$partywlean=="rep")
conj.data.list<-list(conj.data.dems=conj.data.dems, conj.data.reps=conj.data.reps)

conj.labels<-c( "15% of pre-tax income","30% of pre-tax income","40% of pre-tax income","50% Democrat, 50% Republican","30% Democrat, 70% Republican","70% Democrat, 30% Republican", "50% White, 50% Nonwhite","75% White, 25% Nonwhite","90% White, 10% Nonwhite","96% White, 4% Nonwhite", "5 out of 10","9 out of 10","10 mins." , "25 mins.","45 mins.","75 mins." , "20% Less Than National Average", "20% More Than National Average", "City, more residential area","City, downtown area","Rural area","Small town","Suburban neighborhood with houses only","Suburban, downtown area")


for (j in 1:2){
	
	conj.data.clean<-na.omit(cbind.data.frame(conj.data.list[[j]]$response,conj.data.list[[j]]$respid ,conj.data.list[[j]][, trait.cols]))
	names(conj.data.clean) <-c("response", "respid",   "house" ,"pres","race", "school",  "commute","crime","place" ) 
	##estimate model
	conj.model<-lm(response ~ house+pres+race+school+commute+crime+place, data= conj.data.clean)
v<-clx(conj.model,1, conj.data.clean$respid)##calculate respondent-clustered SEs

##extract results, inserting 0s for omitted categories (for plotting):
coefs<-c(0, v["house30 percent of pre-tax income ","Estimate"],  v["house40 percent of pre-tax income ","Estimate"],0,v["pres30% Democrat, 70% Republican ", "Estimate"],v["pres70% Democrat, 30% Republican ","Estimate"],0,v["race75% White, 25% Nonwhite ","Estimate"],v["race90% White, 10% Nonwhite ","Estimate"],v["race96% White, 4% Nonwhite ","Estimate"],0,v["school9 ","Estimate"],0,v["commute25 min ","Estimate"],v["commute45 min ","Estimate"],v["commute75 min ", "Estimate"],0,v["crime20% More Crime Than National Average","Estimate"],0,v["placeCity – downtown, with a mix of offices, apartments, and shops ", "Estimate"],v["placeRural area ","Estimate"],v["placeSmall town ", "Estimate"],v["placeSuburban neighborhood with houses only ", "Estimate"],v["placeSuburban neighborhood with mix of shops, houses, businesses ", "Estimate"])

ses<-c(0, v["house30 percent of pre-tax income ","Std. Error"],  v["house40 percent of pre-tax income ","Std. Error"],0,v["pres30% Democrat, 70% Republican ", "Std. Error"],v["pres70% Democrat, 30% Republican ","Std. Error"],0,v["race75% White, 25% Nonwhite ","Std. Error"],v["race90% White, 10% Nonwhite ","Std. Error"],v["race96% White, 4% Nonwhite ","Std. Error"],0,v["school9 ","Std. Error"],0,v["commute25 min ","Std. Error"],v["commute45 min ","Std. Error"],v["commute75 min ", "Std. Error"],0,v["crime20% More Crime Than National Average","Std. Error"],0,v["placeCity – downtown, with a mix of offices, apartments, and shops ", "Std. Error"],v["placeRural area ","Std. Error"],v["placeSmall town ", "Std. Error"],v["placeSuburban neighborhood with houses only ", "Std. Error"],v["placeSuburban neighborhood with mix of shops, houses, businesses ", "Std. Error"])



if (j==1){	###Democrats	
##make results data frame
label<-rep("Dem.",24)
order<-seq(2,49,by=2)
keep<-c(1,0,0,1,0,0,1,0,0,0,1,0,1,0,0,0,1,0,1,0,0,0,0,0)
conj.dem<-cbind.data.frame(label,as.c(conj.labels), as.n(as.c(coefs)),as.n(as.c(ses)),keep,order)
colnames(conj.dem)<-c("label","item","coef","se","keep","order")}

###Republicans
if (j==2){	
##make results data frame
label<-rep("Rep.",24)
order<-seq(1,48,by=2)
keep<-rep(0,24)
conj.rep<-cbind.data.frame(label,as.c(conj.labels), as.n(as.c(coefs)),as.n(as.c(ses)),keep,order)
colnames(conj.rep)<-c("label","item","coef","se","keep","order")}
}


##make the graph for core results
conj.graph<-rbind.data.frame(conj.dem, conj.rep)
conj.graph<-subset(conj.graph,conj.graph$keep!=1 )##eliminate duplicate zeros
conj.graph.sort<-conj.graph[order(conj.graph$order),]
conj.graph.sort ##ordered so Reps always listed first within category
conj.graph.sort

file.name="conj.graph.sort.Rdata"
save(conj.graph.sort, file=paste(output,file.name,sep=""))



########
#### GRAPH CORE RESULTS
########
######

##plot core conjoint results
conj.graph.sort$colors<-NA
conj.graph.sort$colors[conj.graph.sort$label=="Rep." & conj.graph.sort$coef!=0]<-"red"
conj.graph.sort$colors[conj.graph.sort$label=="Dem." & conj.graph.sort$coef!=0]<-"blue"
conj.graph.sort$colors[conj.graph.sort$label=="Rep." & conj.graph.sort$coef==0]<-"black"

conj.graph.sort$points<-NA
conj.graph.sort$points[conj.graph.sort$colors=="red"]<-19
conj.graph.sort$points[conj.graph.sort$colors=="blue"]<-17
conj.graph.sort$points[conj.graph.sort$colors=="black"]<-15

coef.vec <- conj.graph.sort$coef
se.vec <- conj.graph.sort$se

 var.names2<- c("15% of pre-tax income" , "30% of pre-tax income"    ,              "40% of pre-tax income"  , "50% Democrat, 50% Republican", "30% Democrat, 70% Republican" ,"70% Democrat, 30% Republican"   , "50% White, 50% Nonwhite"  , "75% White, 25% Nonwhite",  "90% White, 10% Nonwhite"  ,  "96% White, 4% Nonwhite"    ,"5 out of 10" ,"9 out of 10"  ,"10 mins."    , "25 mins." , "45 mins.",    "75 mins.", "20% < National Average" , "20% > National Average"   ,  "City, more residential area"   ,  "City, downtown area"   ,  "Rural area "    , "Small town" , "Suburban, only houses" ,   "Suburban, downtown area"  )
 

len(coef.vec)==len(se.vec)
len(se.vec)==len(colors)


y.axis<-rev(c(1, 1.25, 2.25, 2.5, 3.5, 3.75, 4.75, 5, 6, 6.25, 7.25, 8.25, 8.5, 9.5, 10.5, 10.75, 11.75, 12, 13, 13.25, 14.25, 15.25, 15.5, 16.5, 17.5, 17.75, 18.75, 19, 20, 20.25, 21.25, 22.25, 22.5, 23.5, 23.75, 24.75, 25.75, 26, 27, 27.25, 28.25))
len(y.axis)

gray.divides<-c(1.75, 3, 4.25, 5.5, 6.75, 7.75, 9, 10, 11.25, 12.5, 13.75, 14.75, 16, 17, 18.25, 19.5, 20.75,21.75, 23, 24.25, 25.25, 26.5, 27.75   )

var.spots<-c((28.25+27.75)/2, (27.75+26.5)/2, (26.5+25.25)/2,(25.25+24.25)/2, (24.25+23)/2, (23+21.75)/2, (21.75+20.75)/2, (20.75+19.5)/2, (19.5+18.25)/2, (18.25+17)/2, (17+16)/2, (16+14.75)/2, (14.75+13.75)/2, (13.75+12.5)/2, (12.5+11.25)/2, (11.25+10)/2, (10+9)/2, (9+7.75)/2, (7.75+6.75)/2, (6.75+5.5)/2, (5.5+4.25)/2, (4.25+3)/2, (3+1.75)/2 , 1.75/2     )

hard.divides<-c(25.25, 21.75, 17, 14.75, 10, 7.75)

y.ticks<-seq(1.075,9.075,by=1)  

filename<-"conjoint.pdf"


##GENERATE FIGURE 2 in manuscript
pdf(file=paste(output,filename,sep=""), height=7, width=7)

par(mar=c(7, 10.5, 3, 8))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Average Marginal Component-Specific Effect", ylab = "", pch = conj.graph.sort$points, cex = .5,
xlim = c(-.25,.15), xaxs = "r", main = "Conjoint Results: Partisans Agree on Community Quality, 
Disagree On Racial/Political Composition, Type of Place", col=conj.graph.sort$colors) 

segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd =  .6, col=conj.graph.sort$colors)
axis(1, at = seq(-.30,.30,by=.1), labels =  c(-.30,-.2,-.1,0,.1,.2,.30), tick = T, tck=-.009, cex.axis = .82, mgp = c(2,.7,0)) 
axis(2, at = c(-2,y.axis), label = FALSE, las = 1, tick = T, ,mgp = c(2,1,0),tck=0, cex.axis = .72) 
axis(2, at = var.spots, label = FALSE, las = 1, tick = T, ,mgp = c(2,1,0),tck=-.01, cex.axis = .72) 
segments(0,-2,0,len(coef.vec) ,lty=2, lwd=1)
axis(4, at = c(-2,y.axis), label = FALSE, las = 1, tick = T, ,mgp = c(2,1,0),tck=0, cex.axis = .82) 
abline(h=gray.divides, lty=1, col="gray")
#
mtext(var.names2, side=2, at=var.spots, las=2, line=.2, cex=.8)
#
#
abline(h=hard.divides, lwd=1, lty=1, col="black")
###internal tick marks
segments(x0=seq(-1,1,by=.1),y0=c(25.25), x1=seq(-1,1,by=.1),y1=c(25.25)-.25,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(21.75), x1=seq(-1,1,by=.1),y1=c(21.75)-.25,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(17), x1=seq(-1,1,by=.1),y1=c(17)-.25,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(14.75), x1=seq(-1,1,by=.1),y1=c(14.75)-.25,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(10), x1=seq(-1,1,by=.1),y1=c(10)-.25,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(7.75), x1=seq(-1,1,by=.1),y1=c(7.75)-.25,  col="black")
####
mtext("Housing Cost",side=4, at=(28.75+25.25)/2,line=.2, las=2, cex=.9)
mtext("Presidential Vote 
(2012)",side=4, line=.2,at=(25.25+21.75)/2, las=2, cex=.9)
mtext("Racial Composition",side=4,line=.2, at=(21.75+17)/2, las=2, cex=.9)
mtext("School Quality",side=4,line=.2, at=(17+14.75)/2, las=2, cex=.9)
mtext("Daily Driving Time",side=4,line=.2, at=(14.75+10)/2, las=2, cex=.9)
mtext("Violent Crime Rate",side=4, line=.2,at=(10+7.75)/2, las=2, cex=.9)
mtext("Type of Place",side=4, line=.2,at=(7.75)/2, las=2, cex=.9)
legend("bottomleft", legend=c("Republicans","Democrats","reference category"), cex=.4, col=c("red","blue","black"), pch=c(19,17,15))

dev.off()
#




###CONJOINT SORTED v. UNSORTED PARTISANS

#########
#########
#########
##### Sorted v. unsorted partisans (conjoint)
#########
#########
#########

trait.cols<-grep("conj", names(conj.data.party))

conj.labels<-c( "15% of pre-tax income","30% of pre-tax income","40% of pre-tax income","50% Democrat, 50% Republican","30% Democrat, 70% Republican","70% Democrat, 30% Republican", "50% White, 50% Nonwhite","75% White, 25% Nonwhite","90% White, 10% Nonwhite","96% White, 4% Nonwhite", "5 out of 10","9 out of 10","10 mins." , "25 mins.","45 mins.","75 mins." , "20% Less Than National Average", "20% More Than National Average", "City, more residential area","City, downtown area","Rural area","Small town","Suburban neighborhood with houses only","Suburban, downtown area")


##sorted partisans
conj.data.libdem.1<-subset(conj.data.party, conj.data.party$partywlean=="dem" & conj.data.party$libdem==1)
conj.data.conrep.1<-subset(conj.data.party, conj.data.party$partywlean=="rep" & conj.data.party$conrep==1)


##unsorted partians
conj.data.libdem.0<-subset(conj.data.party, conj.data.party$partywlean=="dem" & conj.data.party$libdem==0)
conj.data.conrep.0<-subset(conj.data.party, conj.data.party$partywlean=="rep" & conj.data.party$conrep==0)

conj.data.list<-list(conj.data.libdem.1=conj.data.libdem.1, conj.data.conrep.1=conj.data.conrep.1,conj.data.libdem.0=conj.data.libdem.0, conj.data.conrep.0=conj.data.conrep.0 )
names(conj.data.list)

##make empty data frames to store results
results.libdem.1<-NA
results.libdem.0<-NA
results.conrep.1<-NA
results.conrep.0<-NA



for (j in 1:4){
conj.data.clean<-na.omit(cbind.data.frame(conj.data.list[[j]]$response,conj.data.list[[j]]$respid ,conj.data.list[[j]][, trait.cols]))
	names(conj.data.clean) <-c("response", "respid",   "house" ,"pres","race", "school",  "commute","crime","place" ) 
	##estimate model
	conj.model<-lm(response ~ house+pres+race+school+commute+crime+place, data= conj.data.clean)
v<-clx(conj.model,1, conj.data.clean$respid)##calculate respondent-clustered SEs

##extract results, inserting 0s for omitted categories (for plotting):
coefs<-c(0, v["house30 percent of pre-tax income ","Estimate"],  v["house40 percent of pre-tax income ","Estimate"],0,v["pres30% Democrat, 70% Republican ", "Estimate"],v["pres70% Democrat, 30% Republican ","Estimate"],0,v["race75% White, 25% Nonwhite ","Estimate"],v["race90% White, 10% Nonwhite ","Estimate"],v["race96% White, 4% Nonwhite ","Estimate"],0,v["school9 ","Estimate"],0,v["commute25 min ","Estimate"],v["commute45 min ","Estimate"],v["commute75 min ", "Estimate"],0,v["crime20% More Crime Than National Average","Estimate"],0,v["placeCity b  downtown, with a mix of offices, apartments, and shops ", "Estimate"],v["placeRural area ","Estimate"],v["placeSmall town ", "Estimate"],v["placeSuburban neighborhood with houses only ", "Estimate"],v["placeSuburban neighborhood with mix of shops, houses, businesses ", "Estimate"])

ses<-c(0, v["house30 percent of pre-tax income ","Std. Error"],  v["house40 percent of pre-tax income ","Std. Error"],0,v["pres30% Democrat, 70% Republican ", "Std. Error"],v["pres70% Democrat, 30% Republican ","Std. Error"],0,v["race75% White, 25% Nonwhite ","Std. Error"],v["race90% White, 10% Nonwhite ","Std. Error"],v["race96% White, 4% Nonwhite ","Std. Error"],0,v["school9 ","Std. Error"],0,v["commute25 min ","Std. Error"],v["commute45 min ","Std. Error"],v["commute75 min ", "Std. Error"],0,v["crime20% More Crime Than National Average","Std. Error"],0,v["placeCity b  downtown, with a mix of offices, apartments, and shops ", "Std. Error"],v["placeRural area ","Std. Error"],v["placeSmall town ", "Std. Error"],v["placeSuburban neighborhood with houses only ", "Std. Error"],v["placeSuburban neighborhood with mix of shops, houses, businesses ", "Std. Error"])



if (j==1){	###liberal dems
label<-rep("Lib. Dem.",24)
order<-seq(3,(4*24),by=4)
keep<-c(1,0,0,1,0,0,1,0,0,0,1,0,1,0,0,0,1,0,1,0,0,0,0,0)
conj.libdem.1<-cbind.data.frame(label,as.c(conj.labels), as.n(as.c(coefs)),as.n(as.c(ses)),keep,order)
colnames(conj.libdem.1)<-c("label","item","coef","se","keep","order")
results.libdem.1<-conj.libdem.1}

if (j==2){	###conservative reps
label<-rep("Con. Rep.",24)
order<-seq(1,(4*24),by=4)
keep<-rep(0,24)
conj.conrep.1<-cbind.data.frame(label,as.c(conj.labels), as.n(as.c(coefs)),as.n(as.c(ses)),keep,order)
colnames(conj.conrep.1)<-c("label","item","coef","se","keep","order")
results.conrep.1<-conj.conrep.1}

###Regular Dems
if (j==3){##Republicans with trait=1	
##make results data frame
label<-rep("Reg. Dem.",24)
order<-seq(4,(4*24),by=4)
keep<-c(1,0,0,1,0,0,1,0,0,0,1,0,1,0,0,0,1,0,1,0,0,0,0,0)
conj.libdem.0<-cbind.data.frame(label,as.c(conj.labels), as.n(as.c(coefs)),as.n(as.c(ses)),keep,order)
colnames(conj.libdem.0)<-c("label","item","coef","se","keep","order")
results.libdem.0<-conj.libdem.0}

if (j==4){##Regular Reps	
##make results data frame
label<-rep("Reg. Rep.",24)
order<-seq(2,(4*24),by=4)
keep<-c(1,0,0,1,0,0,1,0,0,0,1,0,1,0,0,0,1,0,1,0,0,0,0,0)
conj.conrep.0<-cbind.data.frame(label,as.c(conj.labels), as.n(as.c(coefs)),as.n(as.c(ses)),keep,order)
colnames(conj.conrep.0)<-c("label","item","coef","se","keep","order")
results.conrep.0<-conj.conrep.0}

}


graph.data.conj.sort<-rbind.data.frame(results.conrep.1, results.conrep.0, results.libdem.1,results.libdem.0 )
graph.data.conj.sort<-subset(graph.data.conj.sort, graph.data.conj.sort$keep!=1)
graph.data.conj.sort<-graph.data.conj.sort[order(graph.data.conj.sort$order),]

graph.data.conj.sort 

graph.data.conj.sort$colors<-NA
graph.data.conj.sort$colors[graph.data.conj.sort$coef==0]<-"black"
graph.data.conj.sort$colors[graph.data.conj.sort$label=="Con. Rep." & graph.data.conj.sort$coef!=0]<-"red"
graph.data.conj.sort$colors[graph.data.conj.sort$label=="Reg. Rep."]<-"pink"
graph.data.conj.sort$colors[graph.data.conj.sort$label=="Lib. Dem."]<-"blue"
graph.data.conj.sort$colors[graph.data.conj.sort$label=="Reg. Dem."]<-"lightskyblue"

graph.data.conj.sort$y.axis<-c(75:1)

conjoint.sorted.partisans.results<-graph.data.conj.sort 


file.name="output/conjoint.sorted.partisans.results.Rdata"
save(conjoint.sorted.partisans.results, file.name, file=file.name)

sorted<-graph.data.conj.sort


sorted$points<-NA
sorted$points[sorted$colors=="red"]<-19
sorted$points[sorted$colors=="pink"]<-19
sorted$points[sorted$colors=="blue"]<-17
sorted$points[sorted$colors=="lightskyblue"]<-17
sorted$points[sorted$colors=="black"]<-15





var.names2<- c("15% of pre-tax income" , "30% of pre-tax income"    ,              "40% of pre-tax income"  , "50% Democrat, 50% Republican", "30% Democrat, 70% Republican" ,"70% Democrat, 30% Republican"   , "50% White, 50% Nonwhite"  , "75% White, 25% Nonwhite",  "90% White, 10% Nonwhite"  ,  "96% White, 4% Nonwhite"    ,"5 out of 10" ,"9 out of 10"  ,"10 mins."    , "25 mins." , "45 mins.",    "75 mins.", "20% < National Average" , "20% > National Average"   ,  "City, more residential area"   ,  "City, downtown area"   ,  "Rural area "    , "Small town" , "Suburban, only houses" ,   "Suburban, downtown area"  )

coef.vec <- as.n(as.character( graph.data.conj.sort$coef))
se.vec <- as.n(as.character(graph.data.conj.sort$se))
y.axis <- c(length(coef.vec):1)


file<-"conjoint.sorted.partisans.pdf"

pdf( paste(output,file,sep=""), height=7, width=7)
par(mar=c(7, 10.5, 3, 8))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Average Marginal Component-Specific Effect", ylab = "", pch = sorted$points, cex = .6,
xlim = c(-.25,.15), xaxs = "r", main = paste("Conjoint Results: Sorted Partisans Drive Most Inter-Party
Differences in Community Preferences",sep=""), col=graph.data.conj.sort 
$colors) 
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd =  .6, col=graph.data.conj.sort 
$colors)
axis(1, at = seq(-.30,.30,by=.1), labels =  c(-.30,-.2,-.1,0,.1,.2,.30), tick = T, tck=-.009, cex.axis = .82, mgp = c(2,.7,0)) 
axis(2, at = c(-2,y.axis), label = FALSE, las = 1, tick = T, ,mgp = c(2,1,0),tck=0, cex.axis = .72) 
segments(0,-2,0,len(coef.vec) ,lty=2, lwd=1)
axis(4, at = c(-2,y.axis), label = FALSE, las = 1, tick = T, ,mgp = c(2,1,0),tck=0, cex.axis = .82) 
abline(h=c(74.5, 70.5, 65.5, 61.5, 56.5, 52.5, 48.5, 43.5, 38.5, 34.5, 30.5, 25.5, 20.5, 16.5, 12.5), lty=1, col="gray")
segments(-.13,8.5,1,8.5 ,lty=1, lwd=1, col="gray")
segments(-.13,4.5,1,4.5 ,lty=1, lwd=1, col="gray")
mtext(var.names2, side=2, at=c(75, (74.5+70.5)/2, (70.5+66.5)/2,66, (65.5+61.5)/2, (61.5+57.5)/2, 57, (56.5+52.5)/2, (52.5+48.5)/2, (48.5+44.5)/2, 44, (43.5+39.5)/2, 39, (38.5+34.5)/2, (34.5+30.5)/2, (30.5+26.5)/2, 26, (25.5+21.5)/2, 21, (20.5+16.5)/2, (16.5+12.5)/2, (12.5+8.5)/2, (8.5+4.5)/2, (4.5+2)/2), las=2, line=.2, cex=.7)
segments(0,-2,0,len(coef.vec) ,lty=2, lwd=1)
abline(h=c(66.5, 57.5, 44.5, 39.5, 26.5, 21.5), lwd=1, lty=1, col="black")
##internal tick marks
segments(x0=seq(-1,1,by=.1),y0=c(66.5), x1=seq(-1,1,by=.1),y1=c(66.5)-.35,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(57.5), x1=seq(-1,1,by=.1),y1=c(57.5)-.35,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(44.5), x1=seq(-1,1,by=.1),y1=c(44.5)-.35,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(39.5), x1=seq(-1,1,by=.1),y1=c(39.5)-.35,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(26.5), x1=seq(-1,1,by=.1),y1=c(26.5)-.35,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(21.5), x1=seq(-1,1,by=.1),y1=c(21.5)-.35,  col="black")
mtext("Housing Cost",side=4, at=(75+66.5)/2,line=.2, las=2, cex=.9)
mtext("Presidential Vote 
(2012)",side=4, line=.2,at=(66.5+57.5)/2, las=2, cex=.9)
mtext("Racial Composition",side=4,line=.2, at=(57.5+44.5)/2, las=2, cex=.9)
mtext("School Quality",side=4,line=.2, at=(44.5+39.5)/2, las=2, cex=.9)
mtext("Daily Driving Time",side=4,line=.2, at=(39.5+26.5)/2, las=2, cex=.9)
mtext("Violent Crime Rate",side=4, line=.2,at=(26.5+21.5)/2, las=2, cex=.9)
mtext("Type of Place",side=4, line=.2,at=(21.5)/2, las=2, cex=.9)

legend("bottomleft", legend=c("reference category","Conservative Rep.","Other Rep.","Liberal Dem.","Other Dem."), cex=.6, col=c("black","red","pink","blue","lightskyblue"), pch=c(15,19,19,17,17))

dev.off()




#################################
#####
##### SUBSETS estimation
#####
#####
#################################

sort.names<-c("white_race","black","female","rich","old","ownrent","educ","homelast5","kidshome","city","black.therm","black.hisp.therm","white.black.diff","single","working","retired","city","same.msa")

subset.data.dem.1<-list(NA)
subset.data.dem.0<-list(NA)
subset.data.rep.1<-list(NA)
subset.data.rep.0<-list(NA)

conj.data.dems<-subset(conj.data.party, conj.data.party$partywlean=="dem")
conj.data.reps<-subset(conj.data.party, conj.data.party$partywlean=="rep")
conj.data.list<-list(conj.data.dems=conj.data.dems, conj.data.reps=conj.data.reps)

###make the subsets
for (j in 1:2){
for (k in 0:1){
for (i in 1:len(sort.names)){##which subset?
	if (j==1 & k==1){###dems with trait==1
	subset.data.dem.1[[i]]<-subset(conj.data.list[[j]],conj.data.list[[j]][,grep(sort.names[i], names(conj.data.list[[j]]), fixed=TRUE)]==k)}
	if (j==1 & k==0){###dems with trait==0
	subset.data.dem.0[[i]]<-subset(conj.data.list[[j]],conj.data.list[[j]][,grep(sort.names[i], names(conj.data.list[[j]]), fixed=TRUE)]==k)}
	if (j==2 & k==1){###reps with trait==1
	subset.data.rep.1[[i]]<-subset(conj.data.list[[j]],conj.data.list[[j]][,grep(sort.names[i], names(conj.data.list[[j]]), fixed=TRUE)]==k)}
	if (j==2 & k==0){###reps with trait==0
	subset.data.rep.0[[i]]<-subset(conj.data.list[[j]],conj.data.list[[j]][,grep(sort.names[i], names(conj.data.list[[j]]), fixed=TRUE)]==k)}
}}}	

##give an intuitive name to each subset of the data
names(subset.data.dem.1)<-paste("dem.", sort.names,".1", sep="")
names(subset.data.dem.0)<-paste("dem.", sort.names,".0", sep="")
names(subset.data.rep.1)<-paste("rep.", sort.names,".1", sep="")
names(subset.data.rep.0)<-paste("rep.", sort.names,".0", sep="")
names(subset.data.dem.1 )


load("dd.final.Rdata")
dd<-dd.final

subset.data<-list(subset.data.dem.1, subset.data.dem.0, subset.data.rep.1, subset.data.rep.0)


##make empty dfs to store results
subset.results.dem.1<-list(NA)
subset.results.dem.0<-list(NA)
subset.results.rep.1<-list(NA)
subset.results.rep.0<-list(NA)


for (j in 1:4){
for (i in 1:len(sort.names)){	
			
conj.data.clean<-na.omit(cbind.data.frame(subset.data[[j]][[i]]$response, subset.data[[j]][[i]]$respid, subset.data[[j]][[i]][, trait.cols],  subset.data[[j]][[i]][,sort.names[i]]))
names(conj.data.clean) <-c("response", "respid",   "house" ,"pres","race", "school",  "commute","crime","place", "sortvar" ) 

	##estimate model
	conj.model<-lm(response ~ house+pres+race+school+commute+crime+place, data= conj.data.clean)
v<-clx(conj.model,1, conj.data.clean$respid)##calculate respondent-clustered SEs

##extract results, inserting 0s for omitted categories (for plotting):
coefs<-c(0, v["house30 percent of pre-tax income ","Estimate"],  v["house40 percent of pre-tax income ","Estimate"],0,v["pres30% Democrat, 70% Republican ", "Estimate"],v["pres70% Democrat, 30% Republican ","Estimate"],0,v["race75% White, 25% Nonwhite ","Estimate"],v["race90% White, 10% Nonwhite ","Estimate"],v["race96% White, 4% Nonwhite ","Estimate"],0,v["school9 ","Estimate"],0,v["commute25 min ","Estimate"],v["commute45 min ","Estimate"],v["commute75 min ", "Estimate"],0,v["crime20% More Crime Than National Average","Estimate"],0,v["placeCity b  downtown, with a mix of offices, apartments, and shops ", "Estimate"],v["placeRural area ","Estimate"],v["placeSmall town ", "Estimate"],v["placeSuburban neighborhood with houses only ", "Estimate"],v["placeSuburban neighborhood with mix of shops, houses, businesses ", "Estimate"])

ses<-c(0, v["house30 percent of pre-tax income ","Std. Error"],  v["house40 percent of pre-tax income ","Std. Error"],0,v["pres30% Democrat, 70% Republican ", "Std. Error"],v["pres70% Democrat, 30% Republican ","Std. Error"],0,v["race75% White, 25% Nonwhite ","Std. Error"],v["race90% White, 10% Nonwhite ","Std. Error"],v["race96% White, 4% Nonwhite ","Std. Error"],0,v["school9 ","Std. Error"],0,v["commute25 min ","Std. Error"],v["commute45 min ","Std. Error"],v["commute75 min ", "Std. Error"],0,v["crime20% More Crime Than National Average","Std. Error"],0,v["placeCity b  downtown, with a mix of offices, apartments, and shops ", "Std. Error"],v["placeRural area ","Std. Error"],v["placeSmall town ", "Std. Error"],v["placeSuburban neighborhood with houses only ", "Std. Error"],v["placeSuburban neighborhood with mix of shops, houses, businesses ", "Std. Error"])




##store results
if (j==1){	###Democrats with trait =1
label<-rep("Dem.",24)
order<-seq(2,49,by=2)
keep<-c(1,0,0,1,0,0,1,0,0,0,1,0,1,0,0,0,1,0,1,0,0,0,0,0)
conj.dem<-cbind.data.frame(label,as.c(conj.labels), as.n(as.c(coefs)),as.n(as.c(ses)),keep,order)
colnames(conj.dem)<-c("label","item","coef","se","keep","order")
subset.results.dem.1[[i]]<-conj.dem}

if (j==2){	###Democrats with trait =0
label<-rep("Dem.",24)
order<-seq(2,49,by=2)
keep<-c(1,0,0,1,0,0,1,0,0,0,1,0,1,0,0,0,1,0,1,0,0,0,0,0)
conj.dem<-cbind.data.frame(label,as.c(conj.labels), as.n(as.c(coefs)),as.n(as.c(ses)),keep,order)
colnames(conj.dem)<-c("label","item","coef","se","keep","order")
subset.results.dem.0[[i]]<-conj.dem}

###Republicans
if (j==3){##Republicans with trait=1	
##make results data frame
label<-rep("Rep.",24)
order<-seq(1,48,by=2)
keep<-rep(0,24)
conj.rep<-cbind.data.frame(label,as.c(conj.labels), as.n(as.c(coefs)),as.n(as.c(ses)),keep,order)
colnames(conj.rep)<-c("label","item","coef","se","keep","order")
subset.results.rep.1[[i]]<-conj.rep}

if (j==4){##Republicans with trait=0	
##make results data frame
label<-rep("Rep.",24)
order<-seq(1,48,by=2)
keep<-rep(0,24)
conj.rep<-cbind.data.frame(label,as.c(conj.labels), as.n(as.c(coefs)),as.n(as.c(ses)),keep,order)
colnames(conj.rep)<-c("label","item","coef","se","keep","order")
subset.results.rep.0[[i]]<-conj.rep}



}
}


graph.data.1<-list(NA)
graph.data.0<-list(NA)

for (i in 1:len(subset.results.dem.1)){
graph.data.1[[i]]<-rbind(subset.results.dem.1[[i]], subset.results.rep.1[[i]])
graph.data.0[[i]]<-rbind(subset.results.dem.0[[i]], subset.results.rep.0[[i]])
}

conjoint.subset.results<-list(graph.data.1, graph.data.0)

for (j in 1:2){
for (i in 1:len(conjoint.subset.results[[1]])){	
conjoint.subset.results[[j]][[i]]<-subset(conjoint.subset.results[[j]][[i]],conjoint.subset.results[[j]][[i]][,"keep"]!=1 )##eliminate duplicate zeros

conjoint.subset.results[[j]][[i]]<-conjoint.subset.results[[j]][[i]][order(conjoint.subset.results[[j]][[i]][,"order"]),]
}}

names(conjoint.subset.results[[1]])<-paste(sort.names, ".1",sep="")
names(conjoint.subset.results[[2]])<-paste(sort.names, ".0",sep="")



##save results
file.name<-"conjoint.subset.results.Rdata"
save(conjoint.subset.results, file=paste(output,file.name,sep=""))


#####GRAPH THE SUBSETS

sort.names<-c("white_race","black","female","rich","old","ownrent","educ","homelast5","kidshome","city","black.therm","black.hisp.therm","white.black.diff","single","working","retired","city","same.msa")



filenames<-list(paste("conjoint.", sort.names, ".1", sep=""), paste("conjoint.", sort.names, ".0", sep=""))

title.trait<-list(c("Whites","Black","Women","Income >$80k/Year","Over 65","Homeowners","Educated","Moved in Last 5 Years", "Those w/ Kids","City Residents","> Median on Black Feeling Therm.","> Median on Black/Hispanic Feeling Therm.","> Median White-Black Feel. Therm Difference","Single","Working","Retired","City Resident","Moved to Same MSA"), c( "Nonwhites" , "Nonblacks", "Men","Income <=$80k/Year","<= 65","Renters","Non-Educated","Not Moved in Last 5 Years", "Those w/out Kids","Non-city Residents","<= Median on Black Feeling Therm.","<= Median on Black/Hispanic Feeling Therm.","<= Median White-Black Feel. Therm Difference","Not Single","Not Working","Not Retired","Not a City Resident","Did Not Move to Same MSA"))

file.name="output/conj.graph.sort.Rdata"
load(file.name)

##plot core conjoint results
conj.graph.sort$colors<-NA
conj.graph.sort$colors[conj.graph.sort$label=="Rep." & conj.graph.sort$coef!=0]<-"red"
conj.graph.sort$colors[conj.graph.sort$label=="Dem." & conj.graph.sort$coef!=0]<-"blue"
conj.graph.sort$colors[conj.graph.sort$label=="Rep." & conj.graph.sort$coef==0]<-"black"

conj.graph.sort$points<-NA
conj.graph.sort$points[conj.graph.sort$colors=="red"]<-19
conj.graph.sort$points[conj.graph.sort$colors=="blue"]<-17
conj.graph.sort$points[conj.graph.sort$colors=="black"]<-15


colors<-conj.graph.sort$colors

colors2<-c("black","red","blue","red","blue","black","red","blue", "red","blue", "black","red","blue", "red","blue", "red","blue",  "black","red","blue",  "black","red","blue", "red","blue", "red","blue", "black","red","blue", "black","red","blue", "red","blue", "red","blue", "red","blue", "red","blue" )

colors==colors2



####LOOP TO MAKE PLOTS OF ALL SUBSETS

for(j in 1:2){##trait = 1 or trait = 0
for(i in 1:len(sort.names) ){ ##which trait?
coef.vec <- as.n(as.character( conjoint.subset.results[[j]][[i]]$coef))
se.vec <- as.n(as.character(conjoint.subset.results[[j]][[i]]$se))

y.1<-seq(length(coef.vec)/2, 1, -1)
y.2<-seq(((length(coef.vec)+1)/2)+.25, 1.25,-1)
y.axis<-c(y.1, y.2)
y.axis<-y.axis[order(y.axis, decreasing=TRUE)] 
len(y.axis)

y.axis<-rev(c(1, 1.25, 2.25, 2.5, 3.5, 3.75, 4.75, 5, 6, 6.25, 7.25, 8.25, 8.5, 9.5, 10.5, 10.75, 11.75, 12, 13, 13.25, 14.25, 15.25, 15.5, 16.5, 17.5, 17.75, 18.75, 19, 20, 20.25, 21.25, 22.25, 22.5, 23.5, 23.75, 24.75, 25.75, 26, 27, 27.25, 28.25))
len(y.axis)


gray.divides<-c(1.75, 3, 4.25, 5.5, 6.75, 7.75, 9, 10, 11.25, 12.5, 13.75, 14.75, 16, 17, 18.25, 19.5, 20.75,21.75, 23, 24.25, 25.25, 26.5, 27.75   )

var.spots<-c((28.25+27.75)/2, (27.75+26.5)/2, (26.5+25.25)/2,(25.25+24.25)/2, (24.25+23)/2, (23+21.75)/2, (21.75+20.75)/2, (20.75+19.5)/2, (19.5+18.25)/2, (18.25+17)/2, (17+16)/2, (16+14.75)/2, (14.75+13.75)/2, (13.75+12.5)/2, (12.5+11.25)/2, (11.25+10)/2, (10+9)/2, (9+7.75)/2, (7.75+6.75)/2, (6.75+5.5)/2, (5.5+4.25)/2, (4.25+3)/2, (3+1.75)/2 , 1.75/2     )

hard.divides<-c(25.25, 21.75, 17, 14.75, 10, 7.75)

y.ticks<-seq(1.075,9.075,by=1)  

file<-paste(output, filenames[[j]][i], ".pdf",sep="")

pdf(file, height=8, width=7)
par(mar=c(7, 10.5, 3, 8))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Average Marginal Component-Specific Effect", ylab = "", pch = conj.graph.sort$points, cex = .5,
xlim = c(-.25,.2), xaxs = "r", main = paste("Conjoint Results:
", title.trait[[j]][i]," Only",sep=""), col=colors) 
##update here to reflect degrees of freedom in each model
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd =  .6, col=colors)
axis(1, at = seq(-.30,.30,by=.1), labels =  c(-.30,-.2,-.1,0,.1,.2,.30), tick = T, tck=-.009, cex.axis = .82, mgp = c(2,.7,0)) 
axis(2, at = c(-2,y.axis), label = FALSE, las = 1, tick = T, ,mgp = c(2,1,0),tck=0, cex.axis = .72) 
axis(2, at = var.spots, label = FALSE, las = 1, tick = T, ,mgp = c(2,1,0),tck=-.01, cex.axis = .72) 
segments(0,-2,0,len(coef.vec) ,lty=2, lwd=1)
axis(4, at = c(-2,y.axis), label = FALSE, las = 1, tick = T, ,mgp = c(2,1,0),tck=0, cex.axis = .82) 

abline(h=gray.divides, lty=1, col="gray")
mtext(var.names2, side=2, at=var.spots, las=2, line=.2, cex=.8)

abline(h=hard.divides, lwd=1, lty=1, col="black")
##internal tick marks
segments(x0=seq(-1,1,by=.1),y0=c(25.25), x1=seq(-1,1,by=.1),y1=c(25.25)-.25,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(21.75), x1=seq(-1,1,by=.1),y1=c(21.75)-.25,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(17), x1=seq(-1,1,by=.1),y1=c(17)-.25,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(14.75), x1=seq(-1,1,by=.1),y1=c(14.75)-.25,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(10), x1=seq(-1,1,by=.1),y1=c(10)-.25,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(7.75), x1=seq(-1,1,by=.1),y1=c(7.75)-.25,  col="black")
####
mtext("Housing Cost",side=4, at=(28.75+25.25)/2,line=.2, las=2, cex=.9)
mtext("Presidential Vote 
(2012)",side=4, line=.2,at=(25.25+21.75)/2, las=2, cex=.9)
mtext("Racial Composition",side=4,line=.2, at=(21.75+17)/2, las=2, cex=.9)
mtext("School Quality",side=4,line=.2, at=(17+14.75)/2, las=2, cex=.9)
mtext("Daily Driving Time",side=4,line=.2, at=(14.75+10)/2, las=2, cex=.9)
mtext("Violent Crime Rate",side=4, line=.2,at=(10+7.75)/2, las=2, cex=.9)
mtext("Type of Place",side=4, line=.2,at=(7.75)/2, las=2, cex=.9)
legend("bottomleft", legend=c("Republicans","Democrats","reference category"), cex=.4, col=c("red","blue","black"), pch=c(19,17,15))
dev.off()
}} 







#########
#########
#########
##### White vs. Non-White Democrats
#########
#########
#########


trait.cols<-grep("conj", names(conj.data.party))

##subset by party
conj.data.white.dem.1<-subset(conj.data.party, conj.data.party$partywlean=="dem" & conj.data.party$white.dem==1)
conj.data.white.dem.0<-subset(conj.data.party, conj.data.party$partywlean=="dem" & conj.data.party$white.dem==0)
conj.data.list<-list(conj.data.white.dem.1=conj.data.white.dem.1, conj.data.white.dem.0=conj.data.white.dem.0)

conj.labels<-c( "15% of pre-tax income","30% of pre-tax income","40% of pre-tax income","50% Democrat, 50% Republican","30% Democrat, 70% Republican","70% Democrat, 30% Republican", "50% White, 50% Nonwhite","75% White, 25% Nonwhite","90% White, 10% Nonwhite","96% White, 4% Nonwhite", "5 out of 10","9 out of 10","10 mins." , "25 mins.","45 mins.","75 mins." , "20% Less Than National Average", "20% More Than National Average", "City, more residential area","City, downtown area","Rural area","Small town","Suburban neighborhood with houses only","Suburban, downtown area")


for (j in 1:2){
	conj.data.clean<-na.omit(cbind.data.frame(conj.data.list[[j]]$response,conj.data.list[[j]]$respid ,conj.data.list[[j]][, trait.cols]))
	names(conj.data.clean) <-c("response", "respid",   "house" ,"pres","race", "school",  "commute","crime","place" ) 
	##estimate model
	conj.model<-lm(response ~ house+pres+race+school+commute+crime+place, data= conj.data.clean)
v<-clx(conj.model,1, conj.data.clean$respid)##calculate respondent-clustered SEs

##extract results, inserting 0s for omitted categories (for plotting):
coefs<-c(0, v["house30 percent of pre-tax income ","Estimate"],  v["house40 percent of pre-tax income ","Estimate"],0,v["pres30% Democrat, 70% Republican ", "Estimate"],v["pres70% Democrat, 30% Republican ","Estimate"],0,v["race75% White, 25% Nonwhite ","Estimate"],v["race90% White, 10% Nonwhite ","Estimate"],v["race96% White, 4% Nonwhite ","Estimate"],0,v["school9 ","Estimate"],0,v["commute25 min ","Estimate"],v["commute45 min ","Estimate"],v["commute75 min ", "Estimate"],0,v["crime20% More Crime Than National Average","Estimate"],0,v["placeCity b  downtown, with a mix of offices, apartments, and shops ", "Estimate"],v["placeRural area ","Estimate"],v["placeSmall town ", "Estimate"],v["placeSuburban neighborhood with houses only ", "Estimate"],v["placeSuburban neighborhood with mix of shops, houses, businesses ", "Estimate"])

ses<-c(0, v["house30 percent of pre-tax income ","Std. Error"],  v["house40 percent of pre-tax income ","Std. Error"],0,v["pres30% Democrat, 70% Republican ", "Std. Error"],v["pres70% Democrat, 30% Republican ","Std. Error"],0,v["race75% White, 25% Nonwhite ","Std. Error"],v["race90% White, 10% Nonwhite ","Std. Error"],v["race96% White, 4% Nonwhite ","Std. Error"],0,v["school9 ","Std. Error"],0,v["commute25 min ","Std. Error"],v["commute45 min ","Std. Error"],v["commute75 min ", "Std. Error"],0,v["crime20% More Crime Than National Average","Std. Error"],0,v["placeCity b  downtown, with a mix of offices, apartments, and shops ", "Std. Error"],v["placeRural area ","Std. Error"],v["placeSmall town ", "Std. Error"],v["placeSuburban neighborhood with houses only ", "Std. Error"],v["placeSuburban neighborhood with mix of shops, houses, businesses ", "Std. Error"])



if (j==1){	###White Democrats	
##make results data frame
label<-rep("White Dem.",24)
order<-seq(2,49,by=2)
keep<-c(1,0,0,1,0,0,1,0,0,0,1,0,1,0,0,0,1,0,1,0,0,0,0,0)
conj.w.dem<-cbind.data.frame(label,as.c(conj.labels), as.n(as.c(coefs)),as.n(as.c(ses)),keep,order)
colnames(conj.w.dem)<-c("label","item","coef","se","keep","order")}

###Non-White Democrats
if (j==2){	
##make results data frame
label<-rep("Non-white Dem.",24)
order<-seq(1,48,by=2)
keep<-rep(0,24)
conj.nw.dem<-cbind.data.frame(label,as.c(conj.labels), as.n(as.c(coefs)),as.n(as.c(ses)),keep,order)
colnames(conj.nw.dem)<-c("label","item","coef","se","keep","order")}
}


##make the graph for core results
conj.graph<-rbind.data.frame(conj.w.dem, conj.nw.dem)
conj.graph<-subset(conj.graph,conj.graph$keep!=1 )##eliminate duplicate zeros
conj.graph.sort<-conj.graph[order(conj.graph$order),]
conj.graph.sort ##ordered so non-white dems always listed first within category
class(conj.graph.sort$label)
conj.graph.sort$label<-as.character(conj.graph.sort$label)



conj.graph<-rbind.data.frame(conj.w.dem, conj.nw.dem)
conj.graph<-subset(conj.graph,conj.graph$keep!=1 )##eliminate duplicate zeros
conj.graph.sort<-conj.graph[order(conj.graph$order),]
conj.graph.sort ##ordered so Reps always listed first within category
conj.graph.sort


file.name="conj.graph.sort.whitedem.Rdata"
save(conj.graph.sort, file=paste(output,file.name,sep=""))




########
#### GRAPH RESULTS
########
######

##plot core conjoint results
conj.graph.sort$colors<-NA
conj.graph.sort$colors[conj.graph.sort$label=="Non-white Dem." & conj.graph.sort$coef!=0]<-"blue"
conj.graph.sort$colors[conj.graph.sort$label=="White Dem." & conj.graph.sort$coef!=0]<-"blue"
conj.graph.sort$colors[conj.graph.sort$label=="Non-white Dem." & conj.graph.sort$coef==0]<-"black"

conj.graph.sort$points<-NA
conj.graph.sort$points[conj.graph.sort$label=="Non-white Dem."]<-19
conj.graph.sort$points[conj.graph.sort$label=="White Dem."]<-17
conj.graph.sort$points[conj.graph.sort$colors=="black"]<-15

coef.vec <- conj.graph.sort$coef
se.vec <- conj.graph.sort$se

conj.graph.sort

 var.names2<- c("15% of pre-tax income" , "30% of pre-tax income"    ,              "40% of pre-tax income"  , "50% Democrat, 50% Republican", "30% Democrat, 70% Republican" ,"70% Democrat, 30% Republican"   , "50% White, 50% Nonwhite"  , "75% White, 25% Nonwhite",  "90% White, 10% Nonwhite"  ,  "96% White, 4% Nonwhite"    ,"5 out of 10" ,"9 out of 10"  ,"10 mins."    , "25 mins." , "45 mins.",    "75 mins.", "20% < National Average" , "20% > National Average"   ,  "City, more residential area"   ,  "City, downtown area"   ,  "Rural area "    , "Small town" , "Suburban, only houses" ,   "Suburban, downtown area"  )
 
#colors<-c("black","red","blue","red","blue","black","red","blue", "red","blue", "black","red","blue", "red","blue", "red","blue",  "black","red","blue",  "black","red","blue", "red","blue", "red","blue", "black","red","blue", "black","red","blue", "red","blue", "red","blue", "red","blue", "red","blue" )

len(coef.vec)==len(se.vec)
len(se.vec)==len(colors)


y.axis<-rev(c(1, 1.25, 2.25, 2.5, 3.5, 3.75, 4.75, 5, 6, 6.25, 7.25, 8.25, 8.5, 9.5, 10.5, 10.75, 11.75, 12, 13, 13.25, 14.25, 15.25, 15.5, 16.5, 17.5, 17.75, 18.75, 19, 20, 20.25, 21.25, 22.25, 22.5, 23.5, 23.75, 24.75, 25.75, 26, 27, 27.25, 28.25))
len(y.axis)

gray.divides<-c(1.75, 3, 4.25, 5.5, 6.75, 7.75, 9, 10, 11.25, 12.5, 13.75, 14.75, 16, 17, 18.25, 19.5, 20.75,21.75, 23, 24.25, 25.25, 26.5, 27.75   )

var.spots<-c((28.25+27.75)/2, (27.75+26.5)/2, (26.5+25.25)/2,(25.25+24.25)/2, (24.25+23)/2, (23+21.75)/2, (21.75+20.75)/2, (20.75+19.5)/2, (19.5+18.25)/2, (18.25+17)/2, (17+16)/2, (16+14.75)/2, (14.75+13.75)/2, (13.75+12.5)/2, (12.5+11.25)/2, (11.25+10)/2, (10+9)/2, (9+7.75)/2, (7.75+6.75)/2, (6.75+5.5)/2, (5.5+4.25)/2, (4.25+3)/2, (3+1.75)/2 , 1.75/2     )

hard.divides<-c(25.25, 21.75, 17, 14.75, 10, 7.75)

y.ticks<-seq(1.075,9.075,by=1)  

filename<-"conjoint.whitedem.pdf"

pdf(file=paste(output,filename,sep=""), height=7, width=7)

par(mar=c(7, 10.5, 3, 8))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Average Marginal Component-Specific Effect", ylab = "", pch = conj.graph.sort$points, cex = .5,
xlim = c(-.25,.15), xaxs = "r", main = "Conjoint Results: White and Non-white Democrats Disagree 
on Racial Composition, Agree on Most Other Traits", col=conj.graph.sort$colors) 

segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd =  .6, col=conj.graph.sort$colors)
axis(1, at = seq(-.30,.30,by=.1), labels =  c(-.30,-.2,-.1,0,.1,.2,.30), tick = T, tck=-.009, cex.axis = .82, mgp = c(2,.7,0)) 
axis(2, at = c(-2,y.axis), label = FALSE, las = 1, tick = T, ,mgp = c(2,1,0),tck=0, cex.axis = .72) 
axis(2, at = var.spots, label = FALSE, las = 1, tick = T, ,mgp = c(2,1,0),tck=-.01, cex.axis = .72) 
segments(0,-2,0,len(coef.vec) ,lty=2, lwd=1)
axis(4, at = c(-2,y.axis), label = FALSE, las = 1, tick = T, ,mgp = c(2,1,0),tck=0, cex.axis = .82) 
abline(h=gray.divides, lty=1, col="gray")
#
mtext(var.names2, side=2, at=var.spots, las=2, line=.2, cex=.8)
#
#
abline(h=hard.divides, lwd=1, lty=1, col="black")
###internal tick marks
segments(x0=seq(-1,1,by=.1),y0=c(25.25), x1=seq(-1,1,by=.1),y1=c(25.25)-.25,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(21.75), x1=seq(-1,1,by=.1),y1=c(21.75)-.25,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(17), x1=seq(-1,1,by=.1),y1=c(17)-.25,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(14.75), x1=seq(-1,1,by=.1),y1=c(14.75)-.25,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(10), x1=seq(-1,1,by=.1),y1=c(10)-.25,  col="black")
segments(x0=seq(-1,1,by=.1),y0=c(7.75), x1=seq(-1,1,by=.1),y1=c(7.75)-.25,  col="black")
####
mtext("Housing Cost",side=4, at=(28.75+25.25)/2,line=.2, las=2, cex=.9)
mtext("Presidential Vote 
(2012)",side=4, line=.2,at=(25.25+21.75)/2, las=2, cex=.9)
mtext("Racial Composition",side=4,line=.2, at=(21.75+17)/2, las=2, cex=.9)
mtext("School Quality",side=4,line=.2, at=(17+14.75)/2, las=2, cex=.9)
mtext("Daily Driving Time",side=4,line=.2, at=(14.75+10)/2, las=2, cex=.9)
mtext("Violent Crime Rate",side=4, line=.2,at=(10+7.75)/2, las=2, cex=.9)
mtext("Type of Place",side=4, line=.2,at=(7.75)/2, las=2, cex=.9)
legend("bottomleft", legend=c("Non-white","White","reference category"), cex=.4, col=c("blue","blue","black"), pch=c(19,17,15))

dev.off()




























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













################################
################################
################################
################################
####### HEAT MAP
################################
################################
################################
################################

library(gplots)

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





###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
##### NEIGHBORHOOD QUALITY PROXY ANALYSIS
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################






load("qual.proxies.Rdata")##county level data on crime, %BA and home-ownership rates
head(qual)
dim(qual)

##subset to places where reporting jurisdictions reported 100 percent of their data
qual2<-qual[qual$COVIND==100, ]
dim(qual2)


cor(qual2$vpc, qual2$pctba, use="complete.obs")##violent crimes per cap and %BA
cor(qual2$vpc, qual2$pctown, use="complete.obs")##violent crimes per cap and home-ownership rate




###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
##### OBSERVED MOVING BEHAVIOR OF SURVEY RESPONDENTS
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################


rm(list=ls()[ls()!="wd" & ls()!="output"])
## Libraries
library(car)
library(fossil)
library(maptools)
library(sp)
library(foreign)
library(zipcode)
library(ggplot2)
library(reshape2)
library(plotrix)
library(gplots)
library(xtable)



load(file="dd.final.Rdata")
dd<-dd.final
dd<-dd[dd$satislife!="",] 


load("zip_apsa_imputed22.Rdata")


dd$knowlastzip<-dd$lastzip
dd$lastzip<-dd$lastzip.text
table(nchar(dd$lastzip.text))

dd$zip5yrs<-dd$lastzip.text
dd$zip5yrs[dd$zip==""]<-NA

table(nchar(dd$zipnew))
table(nchar(dd$zip5yrs))
table(nchar(zip$zipnew))

##get mccain support in current zip
dim(dd)
dd<-merge(dd, zip[,c("zipnew","pctr082")], by="zipnew",all.x=T)
dim(dd)

zip$pctr082.prev<-zip$pctr082

##get mccain support in previous zip
dim(dd)
dd<-merge(dd, zip[,c("zipnew","pctr082.prev")], by.x="zip5yrs",by.y="zipnew",all.x=T)
dim(dd)

head(dd)


##Code relevant zip code variables
dd$pctd08zipnow<-1-dd$pctr082
dd$pctd08zip5yr<-1-dd$pctr082.prev
dd$pctr08zipchg<-dd$pctd08zipnow-dd$pctd08zip5yr


##Estimation


codestr<-c("", "& dmt$white==\'1\'", "& dmt$single==0", "& dmt$single==1", "& dmt$age<=35", "& dmt$age>=35 & dmt$age<=65","& dmt$age>65", "& dmt$income%in%c(\'1.30kless\',\'2.30to39k\')", 
"& dmt$income%in% c(\'7.80to89k\', \'8.90to99k\', \'9.100to119k\', \'10.120kplus\')", "& (dmt$libdem==1|dmt$conrep==1)", "& dmt$partystrength==1", "& dmt$kidshome==1", "&dmt$thrm.urbanites>=75", "&dmt$thrm.rurals>=75","&dmt$same.msa==0","&dmt$same.msa==1")


filestr<-c("", ".white", ".age18to35", ".age35to60", ".incomeunder40k", ".incomeover80k", ".sortedpartisan", ".haskids", ".prourbanite", ".prorurals")
titlestr<-c("All", "Whites Only", "Not Single","Single", "Respondents Age 18-35", "Respondents Age 35-65", "Respondents Age Over 65", "Income Under $40,000", "Income $80,000 and Up", 
"Ideological Partisans", "Strong Partisans", "Children at Home", "Urban People Therm >75", "Rural People Therm >75","Moved Outside MSA","Moved Within MSA")

## Save average sorting for each group.
d.avgnow<-d.avg5yr<-r.avgnow<-r.avg5yr<-rep(NA, length(codestr))
d.avgchg<-r.avgchg<-rep(NA, length(codestr))
d.cilo<-d.cihi<-r.cilo<-r.cihi<-rep(NA, length(codestr))
d.cilo.now<-d.cihi.now<-r.cilo.now<-r.cihi.now<-rep(NA, length(codestr))
d.cilo.5yr<-d.cihi.5yr<-r.cilo.5yr<-r.cihi.5yr<-rep(NA, length(codestr))

dm<-dd[dd$zip5yr!=dd$zip&!is.na(dd$zip5yr),]
dm.dem<-dm[dm$party=="dem",]
dm.rep<-dm[dm$party=="rep",]

dm$partystrength<-as.numeric(dm$partystrengthd=="strong"|dm$partystrengthr=="strong")
table(dm$partystrength)


for(i in 1:length(codestr)){
  dmt<-dm
  dmt$pctd08zipchg<-dmt$pctd08zipnow-dmt$pctd08zip5yr
  dmt.dem<-eval(parse(text=paste("dmt[dmt$party==\'dem\'", codestr[i], ",]", sep="")))
  dmt.rep<-eval(parse(text=paste("dmt[dmt$party==\'rep\'", codestr[i], ",]", sep="")))  
  d.avgnow[i]<-mean(dmt.dem$pctd08zipnow, na.rm=T)
  r.avgnow[i]<-mean(dmt.rep$pctd08zipnow, na.rm=T)
  d.avg5yr[i]<-mean(dmt.dem$pctd08zip5yr, na.rm=T)
  r.avg5yr[i]<-mean(dmt.rep$pctd08zip5yr, na.rm=T)
  d.avgchg[i]<-mean(dmt.dem$pctd08zipchg, na.rm=T)
  r.avgchg[i]<-mean(dmt.rep$pctd08zipchg, na.rm=T)                                                                                          
  d.cilo[i]<-t.test(dmt.dem$pctd08zipchg)$conf.int[1]
  d.cihi[i]<-t.test(dmt.dem$pctd08zipchg)$conf.int[2]
  r.cilo[i]<-t.test(dmt.rep$pctd08zipchg)$conf.int[1]
  r.cihi[i]<-t.test(dmt.rep$pctd08zipchg)$conf.int[2]
  d.cilo.now[i]<-t.test(dmt.dem$pctd08zipnow)$conf.int[1]
  d.cihi.now[i]<-t.test(dmt.dem$pctd08zipnow)$conf.int[2]
  r.cilo.now[i]<-t.test(dmt.rep$pctd08zipnow)$conf.int[1]
  r.cihi.now[i]<-t.test(dmt.rep$pctd08zipnow)$conf.int[2]
  d.cilo.5yr[i]<-t.test(dmt.dem$pctd08zip5yr)$conf.int[1]
  d.cihi.5yr[i]<-t.test(dmt.dem$pctd08zip5yr)$conf.int[2]
  r.cilo.5yr[i]<-t.test(dmt.rep$pctd08zip5yr)$conf.int[1]
  r.cihi.5yr[i]<-t.test(dmt.rep$pctd08zip5yr)$conf.int[2]
}


# Mean change in partisan context, by % Dem, % Republican
grps<-data.frame(grp=codestr, d.chg=d.avgchg, d.cilo=d.cilo, d.cihi=d.cihi, 
r.chg=r.avgchg, r.cilo=r.cilo, r.cihi=r.cihi, d.avgnow=d.avgnow, d.cilo.now=d.cilo.now, d.cihi.now=d.cihi.now, 
r.avgnow=r.avgnow, r.cilo.now=r.cilo.now, r.cihi.now=r.cihi.now, d.avg5yr=d.avgnow, d.cilo.5yr=d.cilo.5yr, d.cihi.5yr=d.cihi.5yr, 
r.avg5yr=r.avgnow, r.cilo.5yr=r.cilo.5yr, r.cihi.5yr=r.cihi.5yr, 
title=titlestr)
grps$diffr<-grps$d.chg-grps$r.chg 
grps$rownum<-seq(nrow(grps), 1)    

## Plotting the change in zip code % Democratic among movers.


##GENERATE FIGURE 6 in manuscript

pdf(paste(output,"movingbehavior.pdf",sep=""), paper='special', height=6, width=6)
par(mar=c(5,12,3,1))
plot(grps$rownum-0.1~grps$d.chg, xlab = "Change in Zip Code Proportion Democratic \n (Among Movers)", ylab = "", main = "Americans Are Not Sorting and \nMost Partisan Subgroups are Mixing",
yaxt="n", las=1, xlim=c(min(grps[,2:7]), max(grps[,2:7])), pch="", col="blue", ylim=c(0.5,max(grps$rownum)+0.7)) 
abline(v=0,lty=2, col="grey")
points(grps$rownum-0.1~grps$d.chg, pch=17, col="blue")
segments(grps$d.cilo, grps$rownum-0.1, grps$d.cihi, grps$rownum-0.1, lwd =.6, col="blue")
points(grps$rownum+0.1~grps$r.chg, pch=19, col="red")
segments(grps$r.cilo, grps$rownum+0.1, grps$r.cihi, grps$rownum+0.1, lwd =.6, col="red")
axis(2, at = grps$rownum, labels = grps$title, las = 1, tick = T, mgp = c(2,1,0), mar=c(2, 12, 2, 2), tck=0, cex.axis = 1.1) 
legend("bottomright", legend=c("Republicans","Democrats"), cex=.7, col=c("red","blue"), pch=c(19,17))
dev.off()





##repeat including those who moved within zipcode

rm(list=ls()[ls()!="wd"&ls()!="output"])
## Libraries
library(car)
library(fossil)
library(maptools)
library(sp)
library(foreign)
library(zipcode)
library(ggplot2)
library(reshape2)
library(plotrix)
library(gplots)
library(xtable)



load(file=("dd.final.Rdata"))
dd<-dd.final
dd<-dd[dd$satislife!="",] 


load("zip_apsa_imputed22.Rdata")


dd$knowlastzip<-dd$lastzip
dd$lastzip<-dd$lastzip.text
table(nchar(dd$lastzip.text))

dd$zip5yrs<-dd$lastzip.text
dd$zip5yrs[dd$zip==""]<-NA

table(nchar(dd$zipnew))
table(nchar(dd$zip5yrs))
table(nchar(zip$zipnew))

##get mccain support in current zip
dim(dd)
dd<-merge(dd, zip[,c("zipnew","pctr082")], by="zipnew",all.x=T)
dim(dd)

zip$pctr082.prev<-zip$pctr082

##get mccain support in previous zip
dim(dd)
dd<-merge(dd, zip[,c("zipnew","pctr082.prev")], by.x="zip5yrs",by.y="zipnew",all.x=T)
dim(dd)

head(dd)


##Code relevant zip code variables
dd$pctd08zipnow<-1-dd$pctr082
dd$pctd08zip5yr<-1-dd$pctr082.prev
dd$pctr08zipchg<-dd$pctd08zipnow-dd$pctd08zip5yr


##Estimation


codestr<-c("", "& dmt$white==\'1\'", "& dmt$single==0", "& dmt$single==1", "& dmt$age<=35", "& dmt$age>=35 & dmt$age<=65","& dmt$age>65", "& dmt$income%in%c(\'1.30kless\',\'2.30to39k\')", 
"& dmt$income%in% c(\'7.80to89k\', \'8.90to99k\', \'9.100to119k\', \'10.120kplus\')", "& (dmt$libdem==1|dmt$conrep==1)", "& dmt$partystrength==1", "& dmt$kidshome==1", "&dmt$thrm.urbanites>=75", "&dmt$thrm.rurals>=75","&dmt$same.msa==0","&dmt$same.msa==1")


filestr<-c("", ".white", ".age18to35", ".age35to60", ".incomeunder40k", ".incomeover80k", ".sortedpartisan", ".haskids", ".prourbanite", ".prorurals")
titlestr<-c("All", "Whites Only", "Not Single","Single", "Respondents Age 18-35", "Respondents Age 35-65", "Respondents Age Over 65", "Income Under $40,000", "Income $80,000 and Up", 
"Ideological Partisans", "Strong Partisans", "Children at Home", "Urban People Therm >75", "Rural People Therm >75","Moved Outside MSA","Moved Within MSA")

## Save average sorting for each group.
d.avgnow<-d.avg5yr<-r.avgnow<-r.avg5yr<-rep(NA, length(codestr))
d.avgchg<-r.avgchg<-rep(NA, length(codestr))
d.cilo<-d.cihi<-r.cilo<-r.cihi<-rep(NA, length(codestr))
d.cilo.now<-d.cihi.now<-r.cilo.now<-r.cihi.now<-rep(NA, length(codestr))
d.cilo.5yr<-d.cihi.5yr<-r.cilo.5yr<-r.cihi.5yr<-rep(NA, length(codestr))

dm<-dd[!is.na(dd$zip5yr),]
dm.dem<-dm[dm$party=="dem",]
dm.rep<-dm[dm$party=="rep",]

dm$partystrength<-as.numeric(dm$partystrengthd=="strong"|dm$partystrengthr=="strong")
table(dm$partystrength)


for(i in 1:length(codestr)){
  dmt<-dm
  dmt$pctd08zipchg<-dmt$pctd08zipnow-dmt$pctd08zip5yr
  dmt.dem<-eval(parse(text=paste("dmt[dmt$party==\'dem\'", codestr[i], ",]", sep="")))
  dmt.rep<-eval(parse(text=paste("dmt[dmt$party==\'rep\'", codestr[i], ",]", sep="")))  
  d.avgnow[i]<-mean(dmt.dem$pctd08zipnow, na.rm=T)
  r.avgnow[i]<-mean(dmt.rep$pctd08zipnow, na.rm=T)
  d.avg5yr[i]<-mean(dmt.dem$pctd08zip5yr, na.rm=T)
  r.avg5yr[i]<-mean(dmt.rep$pctd08zip5yr, na.rm=T)
  d.avgchg[i]<-mean(dmt.dem$pctd08zipchg, na.rm=T)
  r.avgchg[i]<-mean(dmt.rep$pctd08zipchg, na.rm=T)                                                                                          
  d.cilo[i]<-t.test(dmt.dem$pctd08zipchg)$conf.int[1]
  d.cihi[i]<-t.test(dmt.dem$pctd08zipchg)$conf.int[2]
  r.cilo[i]<-t.test(dmt.rep$pctd08zipchg)$conf.int[1]
  r.cihi[i]<-t.test(dmt.rep$pctd08zipchg)$conf.int[2]
  d.cilo.now[i]<-t.test(dmt.dem$pctd08zipnow)$conf.int[1]
  d.cihi.now[i]<-t.test(dmt.dem$pctd08zipnow)$conf.int[2]
  r.cilo.now[i]<-t.test(dmt.rep$pctd08zipnow)$conf.int[1]
  r.cihi.now[i]<-t.test(dmt.rep$pctd08zipnow)$conf.int[2]
  d.cilo.5yr[i]<-t.test(dmt.dem$pctd08zip5yr)$conf.int[1]
  d.cihi.5yr[i]<-t.test(dmt.dem$pctd08zip5yr)$conf.int[2]
  r.cilo.5yr[i]<-t.test(dmt.rep$pctd08zip5yr)$conf.int[1]
  r.cihi.5yr[i]<-t.test(dmt.rep$pctd08zip5yr)$conf.int[2]
}


# Mean change in partisan context, by % Dem, % Republican
grps<-data.frame(grp=codestr, d.chg=d.avgchg, d.cilo=d.cilo, d.cihi=d.cihi, 
r.chg=r.avgchg, r.cilo=r.cilo, r.cihi=r.cihi, d.avgnow=d.avgnow, d.cilo.now=d.cilo.now, d.cihi.now=d.cihi.now, 
r.avgnow=r.avgnow, r.cilo.now=r.cilo.now, r.cihi.now=r.cihi.now, d.avg5yr=d.avgnow, d.cilo.5yr=d.cilo.5yr, d.cihi.5yr=d.cihi.5yr, 
r.avg5yr=r.avgnow, r.cilo.5yr=r.cilo.5yr, r.cihi.5yr=r.cihi.5yr, 
title=titlestr)
grps$diffr<-grps$d.chg-grps$r.chg 
grps$rownum<-seq(nrow(grps), 1)    




pdf(paste(output,"movingbehavior_samezip.pdf",sep=""), paper='special', height=6, width=7)
par(mar=c(5,12,3,1))
plot(grps$rownum-0.1~grps$d.chg, xlab = "Change in Zip Code Proportion Democratic \n (Among Movers)", ylab = "", main = "Americans Are Not Sorting and \nMost Partisan Subgroups are Mixing",
yaxt="n", las=1, xlim=c(min(grps[,2:7]), max(grps[,2:7])), pch="", col="blue", ylim=c(0.5,max(grps$rownum)+0.7)) 
abline(v=0,lty=2, col="grey")
points(grps$rownum-0.1~grps$d.chg, pch=17, col="blue")
segments(grps$d.cilo, grps$rownum-0.1, grps$d.cihi, grps$rownum-0.1, lwd =.6, col="blue")
points(grps$rownum+0.1~grps$r.chg, pch=19, col="red")
segments(grps$r.cilo, grps$rownum+0.1, grps$r.cihi, grps$rownum+0.1, lwd =.6, col="red")
axis(2, at = grps$rownum, labels = grps$title, las = 1, tick = T, mgp = c(2,1,0), mar=c(2, 12, 2, 2), tck=0, cex.axis = 1.1) 
legend("bottomright", legend=c("Republicans","Democrats"), cex=.7, col=c("red","blue"), pch=c(19,17))
dev.off()








###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
####### COMMUNITY PREFERENCE FACTORIAL DESIGN
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################

load("dd.final.Rdata")
dd<-dd.final
dim(dd)


## Community preference design
####
####
###COMM PREF items: create single response vector (0's and 1's) and accompanying treatment factor vector
#####
####

as.n<-as.numeric
as.c<-as.character

table(dd$comm.pref.arm)
## 1st response variable, dichotomous

##test to see everyone only got 1 of 9 conditions
dd$comm.resp.test<-paste(dd$rp.will.prefa1,dd$rp.will.prefa2,dd$rp.will.prefa3, dd$rp.will.prefa4,dd$rp.will.prefa5,dd$rp.will.prefa6,dd$rp.will.prefa7,dd$rp.will.prefa8,dd$rp.will.prefa9, sep="")

table(dd$comm.resp.test)
dd$comm.resp<-as.n(as.c(dd$comm.resp.test))
table(dd$comm.resp)

##second answer to comm pref items
dd$comm.resp2.test<-paste(dd$rp.attr.prefa1,dd$rp.attr.prefa2,dd$rp.attr.prefa3, dd$rp.attr.prefa4,dd$rp.attr.prefa5,dd$rp.attr.prefa6,dd$rp.attr.prefa7,dd$rp.attr.prefa8,dd$rp.attr.prefa9, sep="")

table(dd$comm.resp2.test) 

dd$comm.resp2<-as.n(as.c(dd$comm.resp2.test))


#####
#####
### MEANS for Comm. Pref. 
#####
#####

as.n<-as.numeric
as.c<-as.character
len<-length

###
comm.treat.arms<-c("Comm Pref A1","Comm Pref A2","Comm Pref A3","Comm Pref A4","Comm Pref A5","Comm Pref A6","Comm Pref A7","Comm Pref A8","Comm Pref A9")
comm.treat.arms2<-c("Control/Control","Control/Dem","Control/Rep","White/Rep","White/Control","White/Dem","Minority/Rep","Minority/Control","Minority/Dem")

comm.results.combined<-list(NA)

comm.results.data<-list(all=dd, dems=subset(dd, dd$partywlean=="dem"), reps=subset(dd, dd$partywlean=="rep"))

##COMM PREF ITEMS - whole sample
for (j in 1:3){
t.mean<-NA
t.lb<-NA
t.ub<-NA
t.stat<-NA
t2.mean<-NA
t2.lb<-NA
t2.ub<-NA
t2.stat<-NA
for (i in 1:len(comm.treat.arms)){
	t1<-t.test((comm.results.data[[j]]$comm.resp[comm.results.data[[j]]$comm.pref.arm==comm.treat.arms[i]]), na.rm=TRUE )
	t2<-t.test((comm.results.data[[j]]$comm.resp2[comm.results.data[[j]]$comm.pref.arm==comm.treat.arms[i]]), na.rm=TRUE )
	t.mean[i]<-as.n(t1$estimate)
    t.lb[i] <-t1$conf.int[1]
	t.ub[i] <-t1$conf.int[2]
    t.stat[i] <-as.n(t1$statistic)
    t2.mean[i]<-as.n(t2$estimate)
    t2.lb[i] <-t2$conf.int[1]
	t2.ub[i] <-t2$conf.int[2]
    t2.stat[i] <-as.n(t2$statistic)
    treat.index<-c(1:9)
    }
if(j==1){
	label<-rep("All",9)
	all.1<-cbind.data.frame(label, treat.index,comm.treat.arms2, t.mean, t.lb, t.ub, t2.mean, t2.lb, t2.ub)
	colnames(all.1)<-c("Sample","Treat Index","TreatArm","Mean1", "LB95", "UB95","Mean 2", "LB95.2", "UB95.2" )
		comm.results.combined[[j]]<-all}
if(j==2){label<-rep("Dem",9)
	dems<-cbind.data.frame(label, treat.index,comm.treat.arms2, t.mean, t.lb, t.ub, t2.mean, t2.lb, t2.ub)
	colnames(dems)<-c("Sample","Treat Index","TreatArm","Mean1", "LB95", "UB95","Mean 2", "LB95.2", "UB95.2" ) 
	comm.results.combined[[j]]<-dems}
if(j==3){label<-rep("Rep",9)
	reps<-cbind.data.frame(label, treat.index,comm.treat.arms2, t.mean, t.lb, t.ub, t2.mean, t2.lb, t2.ub)
	colnames(reps)<-c("Sample","Treat Index","TreatArm","Mean1", "LB95", "UB95","Mean 2", "LB95.2", "UB95.2")  
	comm.results.combined[[j]]<-reps}
	}

##save the sorted table 
file.name<-paste(paste(output, "comm.pref.results.Rdata",sep=""))
save(comm.results.combined, file=file.name,row.names=T)



##order results for graphing
comm.results.combined[[2]]$sort.num<-seq(2,2*len(comm.results.combined[[2]][,1]), by=2)
comm.results.combined[[3]]$sort.num<-seq(1,2*len(comm.results.combined[[2]][,1])-1, by=2)
graph.data<-rbind.data.frame(comm.results.combined[[2]], comm.results.combined[[3]])
graph.data<-graph.data[order(graph.data$sort.num), ]
graph.data

##
##NO party info
##
graph.data$new.order<-NA
graph.data$new.order[graph.data$TreatArm=="Control/Control" & graph.data$Sample=="Rep"]<-1
graph.data$new.order[graph.data$TreatArm=="Control/Control" & graph.data$Sample=="Dem"]<-2
graph.data$new.order[graph.data$TreatArm=="Minority/Control" & graph.data$Sample=="Rep"]<-3
graph.data$new.order[graph.data$TreatArm=="Minority/Control" & graph.data$Sample=="Dem"]<-4
graph.data$new.order[graph.data$TreatArm=="White/Control" & graph.data$Sample=="Rep"]<-5
graph.data$new.order[graph.data$TreatArm=="White/Control" & graph.data$Sample=="Dem"]<-6

###
##OWN PARTY
###
##no race/own party
graph.data$new.order[graph.data$TreatArm=="Control/Rep" & graph.data$Sample=="Rep"]<-7
graph.data$new.order[graph.data$TreatArm=="Control/Dem" & graph.data$Sample=="Dem"]<-8
##70% white/own party
graph.data$new.order[graph.data$TreatArm=="Minority/Rep" & graph.data$Sample=="Rep"]<-9
graph.data$new.order[graph.data$TreatArm=="Minority/Dem" & graph.data$Sample=="Dem"]<-10
##96% white/own party
graph.data$new.order[graph.data$TreatArm=="White/Rep" & graph.data$Sample=="Rep"]<-11
graph.data$new.order[graph.data$TreatArm=="White/Dem" & graph.data$Sample=="Dem"]<-12

###
## OUT PARTY
##

#none/out party
graph.data$new.order[graph.data$TreatArm=="Control/Dem" & graph.data$Sample=="Rep"]<-13
graph.data$new.order[graph.data$TreatArm=="Control/Rep" & graph.data$Sample=="Dem"]<-14
##minority/out party
graph.data$new.order[graph.data$TreatArm=="Minority/Dem" & graph.data$Sample=="Rep"]<-15
graph.data$new.order[graph.data$TreatArm=="Minority/Rep" & graph.data$Sample=="Dem"]<-16
#white/out-party
graph.data$new.order[graph.data$TreatArm=="White/Dem" & graph.data$Sample=="Rep"]<-17
graph.data$new.order[graph.data$TreatArm=="White/Rep" & graph.data$Sample=="Dem"]<-18

graph.data

graph.data<-graph.data[order(graph.data$new.order), ]
comm.graph.data<-graph.data

file.name<-paste(output, "comm.graph.data.Rdata",sep="")
save(comm.graph.data, file=file.name,row.names=T)



file.name<-paste(output, "comm.graph.data.Rdata",sep="")

load(file.name)

##display the data that is to be plotted
comm.graph.data

graph.data<-comm.graph.data

##make diff shapes for dems and reps
graph.data$points<-NA
graph.data$points[graph.data$Sample=="Rep"]<-19
graph.data$points[graph.data$Sample=="Dem"]<-17



##make graphical parameters
coef.vec<-as.n(as.c(graph.data[,"Mean1"]))
lb.vec<- as.n(as.c(graph.data[,"LB95"]))
ub.vec<- as.n(as.c(graph.data[,"UB95"]))
#y.axis <- c(length(coef.vec):1)
y.1<-seq(length(coef.vec)/2, 1, -1)
y.2<-seq((length(coef.vec)/2)+.25, 1.25,-1)
y.axis<-c(y.1, y.2)
y.axis<-y.axis[order(y.axis, decreasing=TRUE)] 
y.axis      

var.names<-c("No Race Info" , "70% White","96% White" ,"No Race Info" ,"70% White" ,"96% White"  ,"No Race Info",     "70% White", "96% White")

file.name<-paste(output,"comm1.pdf",sep="")


##calculate the mean response for dems and reps (combined)respondents in the control condition

mean.control<-((graph.data$Mean1[graph.data$TreatArm=="Control/Control" &graph.data$Sample=="Rep" ]) + (graph.data$Mean1[graph.data$TreatArm=="Control/Control" &graph.data$Sample=="Dem" ]))/2

hard.divides<-c(6.575,3.575) 
gray.divides<-seq(2.575,8.575,by=1)
y.ticks<-seq(1.075,9.075,by=1)  

##make the commpref figure, saving as a pdf
pdf(file=file.name, height=4.5)
par(mar=c(5, 10, 5,5))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Mean Willingness to Move to Community (0-1)", ylab = "", pch = graph.data$points, cex = .7, xlim = c(min(lb.vec),max(ub.vec)),   main = "Partisan Composition Strongly Affects Evaluations
of Desirable Communities", col=c("red","blue") )
segments(lb.vec, y.axis, ub.vec, y.axis, lwd =  .7, col=c("red","blue"))
segments(mean.control,0,mean.control, 18, lwd =  .7, lty=2)
axis(1, at = seq(0,1,.1), labels = T, tick = T, tck=-0.03, cex.axis = .8, mgp = c(2,.7,0)) 
axis(2, at = seq(18,0,-1), label =F, las = 1, tick = T, mgp = c(2,1,0),tck=-0, cex.axis = .8) 
axis(2, at =hard.divides, label =F, las = 1, tick = T, mgp = c(2,1,0),tck=-.4, cex.axis = .8)
axis(2, at =y.ticks, label =F, las = 1, tick = T, mgp = c(2,1,0),tck=-.01, cex.axis = .8) 
abline(h=gray.divides, lty=1, lwd=1, col="gray")
segments(0,1.575,.883, 1.575, lwd =  1, lty=1, col="gray")

abline(h=hard.divides, lty=1, lwd=1, col="black")
mtext(var.names,side=2, at=seq(9.075,1.075,by=-1),line=.2, las=2, cex=.9)
mtext(c("No  
Party 
Info &: ","70% 
Own 
Party &: ","70% 
Other 
Party &: "),side=2, at=c((max(y.axis)+hard.divides[1])/2, (hard.divides[1]+hard.divides[2])/2, hard.divides[2]/2), las=2,line=5.5, cex=.9)
legend("bottomright", legend=c("Republicans","Democrats"), cex=.6, col=c("red","blue"), pch=graph.data$points)

dev.off()







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

rm(list=ls()[ls()!="wd" & ls()!="output"])
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




rm(list=ls()[ls()!="wd" & ls()!="output"])
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

   









#######################################
####SCREEN ANALYSIS
#####
#######################################
rm(list=ls()[ls()!="wd" & ls()!="output"])

load("dd.final.Rdata")
dd<-dd.final
head(dd)
#####
#### Run balance tests based on screen items

table(dd$post.screen1)##multiple choice item correct=1; else 0
table(dd$post.screen2.new)##those who enetred at least one character =1; else 0

##balance vars

balance.vars<-cbind.data.frame("Age"=dd$age,  "Income"=dd$income.num,  "Education"=dd$educ2, "Female"=dd$female, "Democrat"=dd$democrat,"Ideology"=dd$ideol, "Interest in Politics"=dd$likepol, "Native English Speaker"=dd$nativeeng, "White"=dd$white2, "Black"=dd$black2, "Hispanic"=dd$hisp2, "Homeowner"=dd$ownrent2,  "Moved in Last 5 years"=dd$homelast5,"Has Kids"=dd$kidshome, "City Resident"=dd$city, "Daily Commute Time"=dd$commuteminwork, "Total Survey Time"=dd$time, post.screen1=dd$post.screen1, post.screen2=dd$post.screen2.new, partywlean=dd$partywlean)

head(balance.vars)
dim(balance.vars)


for (i in seq(1,ncol(balance.vars)-1, by=1)){
	balance.vars[,i]<-as.numeric(as.character(balance.vars[,i]))
	}


head(balance.vars)


##########
##########
##DESCRIPTIVE STATS
##########
##########
##########



balance.vars.party<-subset(balance.vars, balance.vars$partywlean!="ind" & balance.vars$partywlean!="oth" & !is.na(balance.vars$partywlean))

balance.vars.screened<-subset(balance.vars, balance.vars$post.screen1==1&post.screen2==1)

balance.vars.unscreened<-subset(balance.vars, balance.vars$post.screen1!=1|post.screen2!=1)

balance.vars.screened.party<-subset(balance.vars, balance.vars$post.screen1==1&post.screen2==1 & 
balance.vars$partywlean!="ind" & balance.vars$partywlean!="oth" & !is.na(balance.vars$partywlean) )

balance.vars.unscreened.party<-subset(balance.vars.unscreened, balance.vars.unscreened$partywlean!="ind" & balance.vars.unscreened$partywlean!="oth" & !is.na(balance.vars.unscreened$partywlean) )

dim(balance.vars)##full data
dim(balance.vars.party)##partisan sample
dim(balance.vars.screened.party)##partisan sample correct on screens
dim(balance.vars.unscreened.party)##partisan sample incorrect on at least one screen



dim(balance.vars.screened.party)

n.full<-NA
n.party<-NA
n.party.screened<-NA
n.party.unscreened<-NA
mean.full<-NA
mean.party<-NA
mean.party.screened<-NA
mean.party.unscreened<-NA
t.stad.diff.parties<-NA



####
##all respondents who answered first screen
###
as.n<-as.numeric
as.c<-as.character
for (i in seq(1,ncol(balance.vars)-3, by=1)){
	n.full[i]<-as.n(table(is.na(balance.vars[,i]))[1])
	n.party[i]<-as.n(table(is.na(balance.vars.party[,i]))[1])
	n.party.screened[i]<-as.n(table(is.na(balance.vars.screened.party[,i]))[1])
	n.party.unscreened[i]<-as.n(table(is.na(balance.vars.unscreened.party[,i]))[1])

	mean.full[i]<-mean(balance.vars[,i], na.rm=TRUE)
	mean.party[i]<-mean(balance.vars.party[,i], na.rm=TRUE)
	mean.party.screened[i]<-mean(balance.vars.screened.party[,i], na.rm=TRUE)
	mean.party.unscreened[i]<-mean(balance.vars.unscreened.party[,i], na.rm=TRUE)
	
	t.stad.diff.parties[i]<-(t.test(balance.vars.screened.party[,i],balance.vars.unscreened.party[,i], na.rm=TRUE ))$statistic
	
	}

variable<-names(balance.vars[,seq(1,ncol(balance.vars)-3, by=1)])

des.stats<-cbind.data.frame(variable=variable, n.party, n.party.screened, n.party.unscreened, mean.party, mean.party.screened, mean.party.unscreened, t.stad.diff.parties)
rownames(des.stats)<-des.stats$variable
des.stats<-des.stats[,-1]
library(xtable)

##TABLE 1 in Online Appendix
print(xtable(des.stats, digits=c(0,0,0,0,2,2,2,2)))











##########ANES ESTIMATION
rm(list=ls()[ls()!="wd" & ls()!="output"])
as.n<-as.numeric
as.c<-as.character

#####
######
#### ANALYSIS
######
#####

load("dd.final.Rdata")
dd<-dd.final
head(dd)

load("anes.12.regions.Rdata")
head(anes.12)

####make data frames
ourdata<-cbind.data.frame("Age"=dd$age, "Education"=dd$educ.years, "Female"=dd$female, "Married"=dd$married, "Homeowner"=dd$ownrent2, "Ideology"=dd$ideol, "Interest in Politics"=dd$likepol,"Voted (2012)"=dd$vote,  "Non-Hispanic White"=dd$white2, "Non-Hispanic Black"=dd$black2, "Hispanic"=dd$hisp2, "Northeast"=dd$northeast, "Midwest"=dd$midwest, "South"=dd$south, "West"=dd$west, "Democrat"=dd$democrat, "Republican"=dd$republican)


anes.2012.data<-cbind.data.frame("Age"=anes.12$age, "Education"=anes.12$educ.years, "Female"=anes.12$female, "Married"=anes.12$married, "Homeowner"=anes.12$ownrent2, "Ideology"=anes.12$ideol,"Interest in Politics"=anes.12$likepol,"Voted (2012)"=anes.12$vote,  "Non-Hispanic White"=anes.12$white,"Non-Hispanic Black"=anes.12$black, "Hispanic"= anes.12$hisp,  "Northeast"=anes.12$northeast, "Midwest"=anes.12$midwest, "South"=anes.12$south, "West"=anes.12$west, "Democrat"=anes.12$democrat, "Republican"=anes.12$republican )

table(names(ourdata)==names(anes.2012.data))##these should match

##make subsets
ourdata.dems<-subset(ourdata, ourdata$Democrat==1)
ourdata.reps<-subset(ourdata, ourdata$Republican==1)
anes.2012.data.dems<-subset(anes.2012.data,anes.2012.data$Democrat==1 )
anes.2012.data.reps<-subset(anes.2012.data,anes.2012.data$Republican==1 )



mean.diff.data<-list(ourdata.dems= ourdata.dems, ourdata.reps=ourdata.reps, anes.2012.data.dems=anes.2012.data.dems, anes.2012.data.reps=anes.2012.data.reps)
names(mean.diff.data)

#######
######


##make results data frame
N.ourdata<-rep(NA, ncol(ourdata)-2)
N.anes.2012<-rep(NA, ncol(ourdata)-2)


mean.ourdata<-rep(NA, ncol(ourdata)-2)
mean.anes2012<-rep(NA, ncol(ourdata)-2)

mean.diff<-rep(NA, ncol(ourdata)-2)
mean.diff.lb<-rep(NA, ncol(ourdata)-2)
mean.diff.ub<-rep(NA, ncol(ourdata)-2)
t.stat.diff<-rep(NA, ncol(ourdata)-2)

results<-cbind.data.frame(variable=names(ourdata[, 1:(ncol(ourdata)-2)]),N.ourdata=N.ourdata,N.anes.2012=N.anes.2012, mean.ourdata=mean.ourdata,mean.anes2012=mean.anes2012, mean.diff=mean.diff, t.stat.diff=t.stat.diff)

results

results.parties<-list(results.dems=results, results.reps=results)


for (i in 1:(ncol(ourdata)-2)){
##dems
results.parties$results.dems$N.ourdata[i]<-as.n(table(is.na(mean.diff.data$ourdata.dems[,i]))[1])
results.parties$results.dems$N.anes.2012[i]<-as.n(table(is.na(mean.diff.data$anes.2012.data.dems[,i]))[1])

a<-t.test(mean.diff.data$ourdata.dems[,i],mean.diff.data$anes.2012.data.dems[,i], na.rm=TRUE)
results.parties$results.dems$mean.ourdata[i]<-as.n(a$estimate[1])
results.parties$results.dems$mean.anes2012[i]<-as.n(a$estimate[2])
results.parties$results.dems$mean.diff[i]<-as.n(a$estimate[1])-as.n(a$estimate[2])
#results.parties$results.dems$mean.diff.lb[i]<-as.n(a$conf.int[1])
#results.parties$results.dems$mean.diff.ub[i]<-as.n(a$conf.int[2])
results.parties$results.dems$t.stat.diff[i]<-as.n(a$statistic)


##reps

results.parties$results.reps$N.ourdata[i]<-as.n(table(is.na(mean.diff.data$ourdata.reps[,i]))[1])
results.parties$results.reps$N.anes.2012[i]<-as.n(table(is.na(mean.diff.data$anes.2012.data.reps[,i]))[1])


b<-t.test(mean.diff.data$ourdata.reps[,i],mean.diff.data$anes.2012.data.reps[,i], na.rm=TRUE)
results.parties$results.reps$mean.ourdata[i]<-as.n(b$estimate[1])
results.parties$results.reps$mean.anes2012[i]<-as.n(b$estimate[2])
results.parties$results.reps$mean.diff[i]<-as.n(b$estimate[1])-as.n(b$estimate[2])
#results.parties$results.reps$mean.diff.lb[i]<-as.n(b$conf.int[1])
#results.parties$results.reps$mean.diff.ub[i]<-as.n(b$conf.int[2])
results.parties$results.reps$t.stat.diff[i]<-as.n(b$statistic)
}
anes.compare<-results.parties

##drop unnecessary columns and make table to include in appendix
head(anes.compare[[1]])

##TABLE 2 in Online Appendix
print(xtable(anes.compare$results.dems, digits=c(1,0,0,0,2,2,2,2) ),include.rownames=FALSE)

##TABLE 3 in Online Appendix
print(xtable(anes.compare$results.reps, digits=c(1,0,0,0,2,2,2,2) ),include.rownames=FALSE)

file.name<-paste(output, "anes.compare.R", sep="")
save(anes.compare, file=file.name)

file.name<-paste(output,"anes.compare.csv", sep="")
write.csv(anes.compare,file=file.name)






#################
################
######
######CORRELATION MATRICES
#################
###################


####code feeling thermometers


###Obama
##our data: 
table(dd$thrm.obama)
##anes.12 
table(anes.12$ftpo_pres)
anes.12$thrm.obama<-anes.12$ftpo_pres<-as.n(as.c(anes.12$ftpo_pres))
anes.12$thrm.obama[anes.12$thrm.obama<0]<-NA
table(anes.12$thrm.obama)



##unions
##our data
table(dd$thrm.unions)
##anes.12
anes.12$thrm.unions<-anes.12$ftgr_unions<-as.n(as.c(anes.12$ftgr_unions))
anes.12$thrm.unions[anes.12$thrm.unions<0]<-NA
table(anes.12$thrm.unions)

## corporate execs/big business
##our data: "corporate execs"
table(dd$thrm.execs)
##anes.12 "big business"
anes.12$thrm.execs<-anes.12$ftgr_bigbus<-as.n(as.c(anes.12$ftgr_bigbus))
anes.12$thrm.execs[anes.12$thrm.execs<0]<-NA
table(anes.12$thrm.execs)

##atheists
##our data:
table(dd$thrm.atheists)
##anes.12
anes.12$thrm.atheists<-anes.12$ftgr_atheists<-as.n(as.c(anes.12$ftgr_atheists))
anes.12$thrm.atheists[anes.12$thrm.atheists<0]<-NA
table( anes.12$thrm.atheists)

##GOP (conservatives)
##our data: GOP
table(dd$trhm.gop)
##conservatives (instead of GOP)
anes.12$thrm.gop<-anes.12$ftgr_cons<-as.n(as.c(anes.12$ftgr_cons))
anes.12$thrm.gop[anes.12$thrm.gop<0]<-NA
table(anes.12$thrm.gop)

##Dem party (liberals)
#our data: Dem Party
table(dd$thrm.dem)
##anes.12 (liberals)
anes.12$thrm.dem<-anes.12$ftgr_liberals<-as.n(as.c(anes.12$ftgr_liberals))
anes.12$thrm.dem[anes.12$thrm.dem<0]<-NA
table(anes.12$thrm.dem)


ourdata.cor<-cbind.data.frame(Atheists=dd$thrm.atheists, Execs=dd$thrm.execs, Unions=dd$thrm.unions, Obama=dd$thrm.obama)

ourdata.cor.matrix<-as.matrix(cor(ourdata.cor, use='complete.obs'))


##Table 3 in Online Appendix
xtable(ourdata.cor.matrix, digits=3)


anes.12.cor.data<-cbind.data.frame( Atheists=anes.12$thrm.atheists, BigBusiness=anes.12$thrm.execs, Unions=anes.12$thrm.unions, Obama=anes.12$thrm.obama)


anes.12.cor.matrix<-as.matrix(cor(anes.12.cor.data, use='complete.obs'))

##Table 4 in Online Appendix
xtable(anes.12.cor.matrix, digits=3)















	
	

	
	