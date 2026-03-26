
#The following replicates analyses with the Canadian Election Study used in Patrick Fournier, Michael Bang Petersen and Stuart Soroka, "The political phenotype of the disgust sensitive: Correlates of a new abbreviated measure of disgust sensitivity," forthcoming in Electoral Studies.

#The dataset included with this script, CES.working.disgust.Rdata, includes only the recoded variables used in the analyses.  Respondent IDs will allow for these data to be merged with the main CES file, however, distributed by the Canadian Election Study team, currently online at http://www.ces-eec.ca.

#Note that some analyses below are run in STATA and then imported to R uisng the RStata package.  Alternatively, STATA syntax can be pasted directly into a STATA syntax window.


##########
#Prep

library(stargazer)
library(effects)
library(RStata)
options("RStata.StataPath" = '/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp')
options("RStata.StataVersion" = 15)

load("CES.working.disgust.Rdata")


##########
#Figure 1

#pdf("figure1.pdf", width = 5, height = 3.8, bg="white")
barplot(table(C$disgust),names.arg=c("Not Disgusting","Unsure","Disgusting"),las=1)
#invisible(dev.off())


##########
#Participation & Ideology 

variables <- c("participation","turnout","left_right")

for (i in variables) {
  var <- i
  D <- C[,c(var,"disgust","female","under35","over54","univgrad","quebec","extravert","agreable","consciencious","neurotic","open")]
  D <- na.omit(D)
  D$var <- D[,var]
  D$var.std <- D$var - mean(D$var)
  D$var.std <- D$var.std / sd(D$var.std)
  D$disgust.std <- D$disgust - mean(D$disgust)
  D$disgust.std <- D$disgust.std / sd(D$disgust.std)
  model1 <- lm(var.std ~ disgust + female + under35 + over54 + univgrad + quebec, data=D)
  assign(var,model1)
}

D <- C[,c("p_votechce","disgust","female","under35","over54","univgrad","quebec","extravert","agreable","consciencious","neurotic","open")]
D <- na.omit(D)
D$disgust_std <- D$disgust - mean(D$disgust)
D$disgust_std <- D$disgust_std / sd(D$disgust_std)
stata_src <- "
mlogit p_votechce disgust female under35 over54 univgrad quebec 
est store model1
mlogit p_votechce disgust female under35 over54 univgrad quebec 
margins, dydx(disgust) post
est store margins1
esttab model1 using ml.model.txt, cells(b(star fmt(3)) se(par fmt(3))) nogaps star(a 0.10 * 0.05 ** 0.01 *** 0.01) r2 tab replace
esttab margins1 using ml.margins.txt,  se nogaps noobs wide tab plain replace"
stata(stata_src,data.in=D)
vote.ml <- read.csv("ml.margins.txt", skip = c(1), sep="\t")
vote.ml <- vote.ml[-1,]

capture.output(stargazer(participation,turnout,left_right,no.space=T,type="html",out="tablea1.html"), file="NUL") #note that results from STATA are not automatically included in this table, they were inputting manually.


##########
#Figure 2

allvars <- c(variables,"vote_con","vote_lib","vote_ndp","vote_bq","vote_gr")
r <- as.data.frame(allvars)
r$coef <- NA
r$se <- NA
r$variables <- as.character(r$allvars)

r$coef[r$variables=="participation"] <- summary(participation)$coefficients[2, 1] 
r$se[r$variables=="participation"] <- summary(participation)$coefficients[2, 2]
r$coef[r$variables=="turnout"] <- summary(turnout)$coefficients[2, 1] 
r$se[r$variables=="turnout"] <- summary(turnout)$coefficients[2, 2] 
r$coef[r$variables=="left_right"] <- summary(left_right)$coefficients[2, 1] 
r$se[r$variables=="left_right"] <- summary(left_right)$coefficients[2, 2] 

r$coef[r$variables=="vote_con"] <- vote.ml$b[1]
r$se[r$variables=="vote_con"] <- vote.ml$se[1]
r$coef[r$variables=="vote_lib"] <- vote.ml$b[2]
r$se[r$variables=="vote_lib"] <- vote.ml$se[2]
r$coef[r$variables=="vote_ndp"] <- vote.ml$b[3]
r$se[r$variables=="vote_ndp"] <- vote.ml$se[3]
r$coef[r$variables=="vote_bq"] <- vote.ml$b[4]
r$se[r$variables=="vote_bq"] <- vote.ml$se[4]
r$coef[r$variables=="vote_gr"] <- vote.ml$b[5]
r$se[r$variables=="vote_gr"] <- vote.ml$se[5]

r$low <- r$coef - 1.96*r$se
r$high <- r$coef + 1.96*r$se

r$list[r$variables=="participation"] <- 10
r$list[r$variables=="turnout"] <- 9
r$list[r$variables=="left_right"] <- 7
r$list[r$variables=="vote_con"] <- 5
r$list[r$variables=="vote_lib"] <- 4
r$list[r$variables=="vote_ndp"] <- 3
r$list[r$variables=="vote_bq"] <- 2
r$list[r$variables=="vote_gr"] <- 1

r$insig <- 0
r$insig[r$low<(-.01) & r$high>(.01)] <- 1
r$insig[r$allvars=="vote_gr"] <- 1

#pdf("figure2.pdf", width = 8, height = 5.5, bg="white")
par(mar=c(5.1,14.1,2.1,2.1))
plot(r$coef,r$list,ann=F,axes=F,xlim=c(-.4,.4),ylim=c(.5,11))
abline(v=0,lty=2,col="gray")
arrows(r$low,r$list,r$high,r$list,angle=90,length=.05,code=3,col="gray")
points(r$coef[r$insig==1],r$list[r$insig==1],pch=15,cex=1.2,col="gray") 
points(r$coef[r$insig==0],r$list[r$insig==0],pch=15,cex=1.4,col="black")
axis(1,col="gray")
axis(2,las=1,at=c(1:11),labels=rev(c("PARTICIPATION", "Political Participation  ","Turnout  ", "IDEOLOGY","Left-Right  ", "VOTE", "Conservative*  ", "Liberal*  ","NDP*  ","Bloc*  ","Green*  ")), col="white")
mtext("Association with Disgust (Standardized DVs)",side=1,line=3,cex=.8)
mtext("Controls: Demographics / * Marginal Effects from Multinomial Logit",side=1,line=4,cex=.7)
#invisible(dev.off())


##########
#Policy Attitudes 

variables <- c("spend_hlth","spend_welf","spend_educ","spend_envi","spend_defn","spend_crim","tax_personal","tax_corporate","climate_change","poorgap.more","gun_control","terrorists","same_sex","immigration")
  
for (i in variables) {
  var <- i
  D <- C[,c(var,"disgust","female","under35","over54","univgrad","quebec","extravert","agreable","consciencious","neurotic","open","left_right")]
  D <- na.omit(D)
  D$var <- D[,var]
  D$var.std <- D$var - mean(D$var)
  D$var.std <- D$var.std / sd(D$var.std)
  D$disgust.std <- D$disgust - mean(D$disgust)
  D$disgust.std <- D$disgust.std / sd(D$disgust.std)
  model1 <- lm(var.std ~ disgust + female + under35 + over54 + univgrad + quebec + left_right, data=D)
  assign(var,model1)
  }  
  
capture.output(stargazer(spend_hlth,spend_welf,spend_educ,spend_envi,spend_defn,spend_crim,tax_personal,tax_corporate,climate_change,poorgap.more,gun_control,terrorists,same_sex,immigration,no.space=T,type="html",out="tablea2.html"), file="NUL")


##########
#Figure 3

r <- as.data.frame(variables)
r$coef <- NA
r$se <- NA
r$variables <- as.character(r$variables)

r$coef[r$variables=="spend_hlth"] <- summary(spend_hlth)$coefficients[2, 1] 
r$se[r$variables=="spend_hlth"] <- summary(spend_hlth)$coefficients[2, 2] 
r$coef[r$variables=="spend_welf"] <- summary(spend_welf)$coefficients[2, 1] 
r$se[r$variables=="spend_welf"] <- summary(spend_welf)$coefficients[2, 2] 
r$coef[r$variables=="spend_educ"] <- summary(spend_educ)$coefficients[2, 1] 
r$se[r$variables=="spend_educ"] <- summary(spend_educ)$coefficients[2, 2] 
r$coef[r$variables=="spend_envi"] <- summary(spend_envi)$coefficients[2, 1] 
r$se[r$variables=="spend_envi"] <- summary(spend_envi)$coefficients[2, 2] 
r$coef[r$variables=="spend_defn"] <- summary(spend_defn)$coefficients[2, 1] 
r$se[r$variables=="spend_defn"] <- summary(spend_defn)$coefficients[2, 2] 
r$coef[r$variables=="spend_crim"] <- summary(spend_crim)$coefficients[2, 1] 
r$se[r$variables=="spend_crim"] <- summary(spend_crim)$coefficients[2, 2] 
r$coef[r$variables=="tax_personal"] <- summary(tax_personal)$coefficients[2, 1] 
r$se[r$variables=="tax_personal"] <- summary(tax_personal)$coefficients[2, 2] 
r$coef[r$variables=="tax_corporate"] <- summary(tax_corporate)$coefficients[2, 1] 
r$se[r$variables=="tax_corporate"] <- summary(tax_corporate)$coefficients[2, 2] 
r$coef[r$variables=="climate_change"] <- summary(climate_change)$coefficients[2, 1] 
r$se[r$variables=="climate_change"] <- summary(climate_change)$coefficients[2, 2] 
r$coef[r$variables=="poorgap.more"] <- summary(poorgap.more)$coefficients[2, 1] 
r$se[r$variables=="poorgap.more"] <- summary(poorgap.more)$coefficients[2, 2] 
r$coef[r$variables=="gun_control"] <- summary(gun_control)$coefficients[2, 1] 
r$se[r$variables=="gun_control"] <- summary(gun_control)$coefficients[2, 2] 
r$coef[r$variables=="terrorists"] <- summary(terrorists)$coefficients[2, 1] 
r$se[r$variables=="terrorists"] <- summary(terrorists)$coefficients[2, 2] 
r$coef[r$variables=="same_sex"] <- summary(same_sex)$coefficients[2, 1] 
r$se[r$variables=="same_sex"] <- summary(same_sex)$coefficients[2, 2] 
r$coef[r$variables=="immigration"] <- summary(immigration)$coefficients[2, 1] 
r$se[r$variables=="immigration"] <- summary(immigration)$coefficients[2, 2] 

r$low <- r$coef - 1.96*r$se
r$high <- r$coef + 1.96*r$se

r$list[r$variables=="spend_hlth"] <- 16
r$list[r$variables=="spend_welf"] <- 15
r$list[r$variables=="spend_educ"] <- 14
r$list[r$variables=="spend_envi"] <- 13
r$list[r$variables=="spend_defn"] <- 12
r$list[r$variables=="spend_crim"] <- 11
r$list[r$variables=="tax_personal"] <- 9
r$list[r$variables=="tax_corporate"] <- 8
r$list[r$variables=="climate_change"] <- 7
r$list[r$variables=="poorgap.more"] <- 6
r$list[r$variables=="gun_control"] <- 4
r$list[r$variables=="terrorists"] <- 3
r$list[r$variables=="same_sex"] <- 2
r$list[r$variables=="immigration"] <- 1


#pdf("figure3.pdf", width = 8, height = 8, bg="white")
par(mar=c(5.1,14.1,2.1,2.1))
plot(r$coef,r$list,ann=F,axes=F,xlim=c(-.4,.4),ylim=c(.5,17))
abline(v=0,lty=2,col="gray")
arrows(r$low,r$list,r$high,r$list,angle=90,length=.05,code=3,col="gray")
r$insig <- 0
r$insig[r$variables=="spend_welf"] <- 1
r$insig[r$variables=="spend_envi"] <- 1
r$insig[r$variables=="spend_defn"] <- 1
r$insig[r$variables=="spend_educ"] <- 1
r$insig[r$variables=="tax_corporate"] <- 1
r$insig[r$variables=="poorgap.more"] <- 1
points(r$coef[r$insig==1],r$list[r$insig==1],pch=15,cex=1.2,col="gray")
points(r$coef[r$insig==0],r$list[r$insig==0],pch=15,cex=1.4,col="black")
axis(1,col="gray")
axis(2,las=1,at=c(1:17),labels=rev(c("GOVT EXPENDITURES","Health Spending  ","Welfare Spending  ","Education Spending  ","Envir Spending  ","Defense Spending  ","Crime Spending  ","TAXATION & REDISTRIBUTION","Personal Tax  ","Corporate Tax  ","Climate Chg Tax  ","Reduce Income Gap  ","SOCIAL POLICY","Gun Control  ","Limit Citizens's Rights  ","Pro Same-Sex Marriage  ", "Admit Immigrants  ")),col="white")
mtext("Association with Disgust (Standardized DVs)",side=1,line=3,cex=.8)
mtext("Controls: Demographics, Ideology",side=1,line=4,cex=.7)
#invisible(dev.off())


##########
#Group Attitudes 

variables <- c("therm_abor","outlanguage1","therm_femi","therm_gays","therm_mino","therm_musl","therm_immg","pers_trust")
  
for (i in variables) {
  var <- i
  D <- C[,c(var,"disgust","female","under35","over54","univgrad","quebec","extravert","agreable","consciencious","neurotic","open","left_right")]
  D <- na.omit(D)
  D$var <- D[,var]
  D$var.std <- D$var - mean(D$var)
  D$var.std <- D$var.std / sd(D$var.std)
  D$disgust.std <- D$disgust - mean(D$disgust)
  D$disgust.std <- D$disgust.std / sd(D$disgust.std)
  model1 <- lm(var.std ~ disgust + female + under35 + over54 + univgrad + quebec + left_right, data=D)
  assign(var,model1)
  }  

capture.output(stargazer(therm_abor,outlanguage1,therm_femi,therm_gays,therm_mino,therm_musl,therm_immg,pers_trust,no.space=T,type="html",out="tablea3.html"), file="NUL")


##########
#Figure 4

r <- as.data.frame(variables)
r$coef <- NA
r$se <- NA

r$coef[r$variables=="therm_abor"] <- summary(therm_abor)$coefficients[2, 1] 
r$se[r$variables=="therm_abor"] <- summary(therm_abor)$coefficients[2, 2]
r$coef[r$variables=="outlanguage1"] <- summary(outlanguage1)$coefficients[2, 1] 
r$se[r$variables=="outlanguage1"] <- summary(outlanguage1)$coefficients[2, 2] 
r$coef[r$variables=="therm_femi"] <- summary(therm_femi)$coefficients[2, 1] 
r$se[r$variables=="therm_femi"] <- summary(therm_femi)$coefficients[2, 2] 
r$coef[r$variables=="therm_gays"] <- summary(therm_gays)$coefficients[2, 1] 
r$se[r$variables=="therm_gays"] <- summary(therm_gays)$coefficients[2, 2] 
r$coef[r$variables=="therm_mino"] <- summary(therm_mino)$coefficients[2, 1] 
r$se[r$variables=="therm_mino"] <- summary(therm_mino)$coefficients[2, 2] 
r$coef[r$variables=="therm_musl"] <- summary(therm_musl)$coefficients[2, 1] 
r$se[r$variables=="therm_musl"] <- summary(therm_musl)$coefficients[2, 2] 
r$coef[r$variables=="therm_immg"] <- summary(therm_immg)$coefficients[2, 1] 
r$se[r$variables=="therm_immg"] <- summary(therm_immg)$coefficients[2, 2] 
r$coef[r$variables=="pers_trust"] <- summary(therm_immg)$coefficients[2, 1] 
r$se[r$variables=="pers_trust"] <- summary(therm_immg)$coefficients[2, 2] 

r$low <- r$coef - 1.96*r$se
r$high <- r$coef + 1.96*r$se

r$list[r$variables=="therm_abor"] <- 9
r$list[r$variables=="outlanguage1"] <- 8
r$list[r$variables=="therm_femi"] <- 7
r$list[r$variables=="therm_gays"] <- 6
r$list[r$variables=="therm_mino"] <- 5
r$list[r$variables=="therm_musl"] <- 4
r$list[r$variables=="therm_immg"] <- 3
r$list[r$variables=="pers_trust"] <- 1

#pdf("figure4.pdf", width = 8, height = 5, bg="white")
par(mar=c(5.1,14.1,2.1,2.1))
plot(r$coef,r$list,ann=F,axes=F,xlim=c(-.4,.4),ylim=c(.5,10))
abline(v=0,lty=2,col="gray")
arrows(r$low,r$list,r$high,r$list,angle=90,length=.05,code=3,col="gray")
r$insig <- 0
r$insig[r$low<(-.01) & r$high>(.01)] <- 1
points(r$coef[r$insig==1],r$list[r$insig==1],pch=15,cex=1.2,col="gray")
points(r$coef[r$insig==0],r$list[r$insig==0],pch=15,cex=1.4,col="black")
axis(1,col="gray")
axis(2,las=1,at=c(1:10),labels=rev(c("GROUP ATTITUDES","Aboriginals  ", "Out-Language Group  ","Feminists  ", "Gays  ","Minorities  ", "Muslims  ", "Immigrants  ", "TRUST", "Social Trust  ")), col="white")
mtext("Association with Disgust (Standardized DVs)",side=1,line=3,cex=.8)
mtext("Controls: Demographics, Ideology",side=1,line=4,cex=.7)
#invisible(dev.off())


##########
#Disgust * Partisanship

gun.model1 <- lm(gun_control ~ disgust * left_right + female + under35 + over54 + univgrad + quebec, data=C)
eff <- effect("disgust * left_right",gun.model1,xlevels=list(left_right=c(0,1)))
eff1r <- cbind(eff$x,fit=eff$fit,lower=eff$lower,higher=eff$upper)
  
tax.model1 <- lm(tax_personal ~ disgust * left_right + female + under35 + over54 + univgrad + quebec , data=C)
eff <- effect("disgust * left_right",tax.model1,xlevels=list(left_right=c(0,1)))
eff2r <- cbind(eff$x,fit=eff$fit,lower=eff$lower,higher=eff$upper)
  
imm.model1 <- lm(immigration ~ disgust * left_right + female + under35 + over54 + univgrad + quebec , data=C)
eff <- effect("disgust * left_right",imm.model1,xlevels=list(left_right=c(0,1)))
eff3r <- cbind(eff$x,fit=eff$fit,lower=eff$lower,higher=eff$upper)
  
capture.output(stargazer(gun.model1,tax.model1,imm.model1,no.space=T,type="html",out="tablea4.html"), file="NUL")


##########
#Figure 5

#pdf("figure5.pdf", width = 10, height =4, bg="white")
par(mfrow=c(1,3)) 
poly <- c(eff1r$disgust[eff1r$left_right==0],rev(eff1r$disgust[eff1r$left_right==0]))
gon <- c(eff1r$lower[eff1r$left_right==0],rev(eff1r$higher[eff1r$left_right==0]))
polyg0 <- as.data.frame(cbind(poly,gon))
poly <- c(eff1r$disgust[eff1r$left_right==1],rev(eff1r$disgust[eff1r$left_right==1]))
gon <- c(eff1r$lower[eff1r$left_right==1],rev(eff1r$higher[eff1r$left_right==1]))
polyg1 <- as.data.frame(cbind(poly,gon))
plot(eff1r$disgust, eff1r$fit,  type="n", ann=F, axes=F, ylim=c(.4,.8)) 
polygon(polyg0$poly,polyg0$gon,border=NA,col="gray",density=35,angle=90)
polygon(polyg1$poly,polyg1$gon,border=NA,col="gray",density=36,angle=90)
lines(eff1r$disgust[eff1r$left_right==0], eff1r$fit[eff1r$left_right==0],  type="l", col="firebrick3", pch=15, lwd=4)
lines(eff1r$disgust[eff1r$left_right==1], eff1r$fit[eff1r$left_right==1],  type="l", col="dodgerblue3", pch=15, lwd=4)
axis(side=2, las=1, cex.axis=.8, col="gray")
mtext(side=2,"Gun Control",cex=1,line=2.5)
axis(side=1, las=1, cex.axis=.8, col="gray", padj=.8)
text(1,.5,"Liberals",col="firebrick3",pos=2)
text(1,.47,"Conservatives",col="dodgerblue3",pos=2)

poly <- c(eff2r$disgust[eff2r$left_right==0],rev(eff2r$disgust[eff2r$left_right==0]))
gon <- c(eff2r$lower[eff2r$left_right==0],rev(eff2r$higher[eff2r$left_right==0]))
polyg0 <- as.data.frame(cbind(poly,gon))
poly <- c(eff2r$disgust[eff2r$left_right==1],rev(eff2r$disgust[eff2r$left_right==1]))
gon <- c(eff2r$lower[eff2r$left_right==1],rev(eff2r$higher[eff2r$left_right==1]))
polyg1 <- as.data.frame(cbind(poly,gon))
plot(eff2r$disgust, eff2r$fit,  type="n", ann=F, axes=F, ylim=c(.15,.4)) 
polygon(polyg0$poly,polyg0$gon,border=NA,col="gray",density=35,angle=90)
polygon(polyg1$poly,polyg1$gon,border=NA,col="gray",density=36,angle=90)
lines(eff2r$disgust[eff2r$left_right==0], eff2r$fit[eff2r$left_right==0],  type="l", col="firebrick3", pch=15, lwd=4)
lines(eff2r$disgust[eff2r$left_right==1], eff2r$fit[eff2r$left_right==1],  type="l", col="dodgerblue3", pch=15, lwd=4)
axis(side=2, las=1, cex.axis=.8, col="gray")
mtext(side=2,"Personal Taxes",cex=1,line=2.5)
axis(side=1, las=1, cex.axis=.8, col="gray", padj=.8)
mtext(side=1,"Disgust Sensivity",cex=1,line=3)

poly <- c(eff3r$disgust[eff3r$left_right==0],rev(eff3r$disgust[eff3r$left_right==0]))
gon <- c(eff3r$lower[eff3r$left_right==0],rev(eff3r$higher[eff3r$left_right==0]))
polyg0 <- as.data.frame(cbind(poly,gon))
poly <- c(eff3r$disgust[eff3r$left_right==1],rev(eff3r$disgust[eff3r$left_right==1]))
gon <- c(eff3r$lower[eff3r$left_right==1],rev(eff3r$higher[eff3r$left_right==1]))
polyg1 <- as.data.frame(cbind(poly,gon))
plot(eff3r$disgust, eff3r$fit,  type="n", ann=F, axes=F, ylim=c(.1,.8)) 
polygon(polyg0$poly,polyg0$gon,border=NA,col="gray",density=35,angle=90)
polygon(polyg1$poly,polyg1$gon,border=NA,col="gray",density=36,angle=90)
lines(eff3r$disgust[eff3r$left_right==0], eff3r$fit[eff3r$left_right==0],  type="l", col="firebrick3", pch=15, lwd=4)
lines(eff3r$disgust[eff3r$left_right==1], eff3r$fit[eff3r$left_right==1],  type="l", col="dodgerblue3", pch=15, lwd=4)
axis(side=2, las=1, cex.axis=.8, col="gray")
mtext(side=2,"Immigration",cex=1,line=2.5)
axis(side=1, las=1, cex.axis=.8, col="gray", padj=.8)
#dev.off()

