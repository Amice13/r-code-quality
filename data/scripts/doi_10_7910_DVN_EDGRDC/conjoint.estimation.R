

#################################
#####
##### CONJOINT ANALYSIS, CORE RESULTS
#####
#####
#################################
cat("Begin CONJOINT ANALYSIS, CORE RESULTS\n")

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
coefs<-c(0, v["house30 percent of pre-tax income ","Estimate"],  v["house40 percent of pre-tax income ","Estimate"],0,v["pres30% Democrat, 70% Republican ", "Estimate"],v["pres70% Democrat, 30% Republican ","Estimate"],0,v["race75% White, 25% Nonwhite ","Estimate"],v["race90% White, 10% Nonwhite ","Estimate"],v["race96% White, 4% Nonwhite ","Estimate"],0,v["school9 ","Estimate"],0,v["commute25 min ","Estimate"],v["commute45 min ","Estimate"],v["commute75 min ", "Estimate"],0,v["crime20% More Crime Than National Average","Estimate"],0,v["placeCity – downtown, with a mix of offices, apartments, and shops ", "Estimate"],v["placeRural area ","Estimate"],v["placeSmall town ", "Estimate"],v["placeSuburban neighborhood with houses only ", "Estimate"],v["placeSuburban neighborhood with mix of shops, houses, businesses ", "Estimate"])

ses<-c(0, v["house30 percent of pre-tax income ","Std. Error"],  v["house40 percent of pre-tax income ","Std. Error"],0,v["pres30% Democrat, 70% Republican ", "Std. Error"],v["pres70% Democrat, 30% Republican ","Std. Error"],0,v["race75% White, 25% Nonwhite ","Std. Error"],v["race90% White, 10% Nonwhite ","Std. Error"],v["race96% White, 4% Nonwhite ","Std. Error"],0,v["school9 ","Std. Error"],0,v["commute25 min ","Std. Error"],v["commute45 min ","Std. Error"],v["commute75 min ", "Std. Error"],0,v["crime20% More Crime Than National Average","Std. Error"],0,v["placeCity – downtown, with a mix of offices, apartments, and shops ", "Std. Error"],v["placeRural area ","Std. Error"],v["placeSmall town ", "Std. Error"],v["placeSuburban neighborhood with houses only ", "Std. Error"],v["placeSuburban neighborhood with mix of shops, houses, businesses ", "Std. Error"])

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
coefs<-c(0, v["house30 percent of pre-tax income ","Estimate"],  v["house40 percent of pre-tax income ","Estimate"],0,v["pres30% Democrat, 70% Republican ", "Estimate"],v["pres70% Democrat, 30% Republican ","Estimate"],0,v["race75% White, 25% Nonwhite ","Estimate"],v["race90% White, 10% Nonwhite ","Estimate"],v["race96% White, 4% Nonwhite ","Estimate"],0,v["school9 ","Estimate"],0,v["commute25 min ","Estimate"],v["commute45 min ","Estimate"],v["commute75 min ", "Estimate"],0,v["crime20% More Crime Than National Average","Estimate"],0,v["placeCity – downtown, with a mix of offices, apartments, and shops ", "Estimate"],v["placeRural area ","Estimate"],v["placeSmall town ", "Estimate"],v["placeSuburban neighborhood with houses only ", "Estimate"],v["placeSuburban neighborhood with mix of shops, houses, businesses ", "Estimate"])

ses<-c(0, v["house30 percent of pre-tax income ","Std. Error"],  v["house40 percent of pre-tax income ","Std. Error"],0,v["pres30% Democrat, 70% Republican ", "Std. Error"],v["pres70% Democrat, 30% Republican ","Std. Error"],0,v["race75% White, 25% Nonwhite ","Std. Error"],v["race90% White, 10% Nonwhite ","Std. Error"],v["race96% White, 4% Nonwhite ","Std. Error"],0,v["school9 ","Std. Error"],0,v["commute25 min ","Std. Error"],v["commute45 min ","Std. Error"],v["commute75 min ", "Std. Error"],0,v["crime20% More Crime Than National Average","Std. Error"],0,v["placeCity – downtown, with a mix of offices, apartments, and shops ", "Std. Error"],v["placeRural area ","Std. Error"],v["placeSmall town ", "Std. Error"],v["placeSuburban neighborhood with houses only ", "Std. Error"],v["placeSuburban neighborhood with mix of shops, houses, businesses ", "Std. Error"])


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
coefs<-c(0, v["house30 percent of pre-tax income ","Estimate"],  v["house40 percent of pre-tax income ","Estimate"],0,v["pres30% Democrat, 70% Republican ", "Estimate"],v["pres70% Democrat, 30% Republican ","Estimate"],0,v["race75% White, 25% Nonwhite ","Estimate"],v["race90% White, 10% Nonwhite ","Estimate"],v["race96% White, 4% Nonwhite ","Estimate"],0,v["school9 ","Estimate"],0,v["commute25 min ","Estimate"],v["commute45 min ","Estimate"],v["commute75 min ", "Estimate"],0,v["crime20% More Crime Than National Average","Estimate"],0,v["placeCity – downtown, with a mix of offices, apartments, and shops ", "Estimate"],v["placeRural area ","Estimate"],v["placeSmall town ", "Estimate"],v["placeSuburban neighborhood with houses only ", "Estimate"],v["placeSuburban neighborhood with mix of shops, houses, businesses ", "Estimate"])

ses<-c(0, v["house30 percent of pre-tax income ","Std. Error"],  v["house40 percent of pre-tax income ","Std. Error"],0,v["pres30% Democrat, 70% Republican ", "Std. Error"],v["pres70% Democrat, 30% Republican ","Std. Error"],0,v["race75% White, 25% Nonwhite ","Std. Error"],v["race90% White, 10% Nonwhite ","Std. Error"],v["race96% White, 4% Nonwhite ","Std. Error"],0,v["school9 ","Std. Error"],0,v["commute25 min ","Std. Error"],v["commute45 min ","Std. Error"],v["commute75 min ", "Std. Error"],0,v["crime20% More Crime Than National Average","Std. Error"],0,v["placeCity – downtown, with a mix of offices, apartments, and shops ", "Std. Error"],v["placeRural area ","Std. Error"],v["placeSmall town ", "Std. Error"],v["placeSuburban neighborhood with houses only ", "Std. Error"],v["placeSuburban neighborhood with mix of shops, houses, businesses ", "Std. Error"])




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

cat("End CONJOINT ANALYSIS, CORE RESULTS\n")


