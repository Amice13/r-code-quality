#!bin/usr/R
# Created 2011-12-25
# Update 2012-05-16
# Climate Change Experiment
# GSU 2011 Online Survey Experiment Data

# All variables precoded -1 to +1, with higher scores meaning agreement
# On GWHuman, 1 means human caused, -1 means naturally caused
# On AdaptPrevent, -1 = Prevention, 0 = Equal parts each, +1 = Adaptation
# On PartyID, -1 = Republican, 0 = Independent, +1 = Democrat


###############################################################################
## ANALYSIS
###############################################################################

library("car")
library("coin")
data1 <- read.csv("gsu2011-data.csv")
data <- subset(data1,attentive==1 & Condition %in% 1:4)
# CREATE PARTY ID INDICATORS
data$partyid.tri = recode(data$partyid,"-1:-.33=-1;0=0;.33:1=1",as.factor.result=TRUE) # indicator for rep (-1), indep (0), dem (1)
data$partyid.bin = recode(data$partyid.tri,"-1=0;0=NA;1=1",as.factor.result=FALSE) # indicator for rep (0), dem(+1)

#.groupNames <- c("Control","Norm_NoSci","NoNorm_Sci","Norm_Sci","NoNorm_PolSci","Norm_PolSci")
.groupNames <- c("Control","Norm_NoSci","NoNorm_Sci","Norm_Sci")


## DESCRIPTIVES 

round(prop.table(table(data$partyid.tri))*100,1)
round(prop.table(table(data$partyid.bin))*100,1)

partydiff <- function(var){
    data$var <- data[,var]
    x1 <- with(subset(data,partyid.bin==0),expResults(var,Condition))
    x2 <- with(subset(data,partyid.bin==1),expResults(var,Condition))
    x3m <- x2[,1]-x1[,1]
    x3s <- sqrt(x2[,4]+x1[,4])
    tab <- cbind(coefpaste(x1[,1],x1[,4]),coefpaste(x2[,1],x2[,4]),coefpaste(x3m,x3s))
    data$var <- NULL
    colnames(tab) <- c("Republicans","Democrats","Difference (D-R)")
    #print(tab,quote=FALSE)
    return(list(x1[,c(1,4)],x2[,c(1,4)],cbind(x3m,x3s)))
}

partydiff("GWBelief")
partydiff("GWHuman")
partydiff("PersonalAction")
partydiff("EmissionCap")
#partydiff("Email")
#partydiff("Increase")
#partydiff("Decrease")

# PLOTS

diffplot <- function(var,title) {
    obj <- partydiff(var)
    plot((1:4)-.1,obj[[1]][,1], pch=15, cex=1.5, xaxt="n", yaxt="n", xlab="", ylab="", bty="n",
        main=title, xlim=c(.5,4.5), ylim=c(-.4,.8)) # Plot Republicans
    axis(1,1:4,c("Control", "Norm-only","Science-only","Norm + Science"),cex=.5) # X-axis
    axis(2,las=1) # Y-axis
    points((1:4)+.1,obj[[2]][,1], pch=19, cex=1.7) # Plot Democrats
    for(i in 1:4){
        segments(i-.1,obj[[1]][i,1]-obj[[1]][i,2],i-.1,obj[[1]][i,1]+obj[[1]][i,2],lwd=2) # Plot Republican SEs
        segments(i+.1,obj[[2]][i,1]-obj[[2]][i,2],i+.1,obj[[2]][i,1]+obj[[2]][i,2],lwd=2) # Plot Democratic SEs
    }
    text(1,obj[[1]][1,1],"Reps",pos=4)
    text(1.2,obj[[2]][1,1],"Dems",pos=4)
}

png("2011_gwbelief.png",width=600,height=400)
diffplot("GWBelief","Belief in Global Warming, by Party Identification")
dev.off()

png("2011_gwhuman.png",width=600,height=400)
diffplot("GWHuman","Belief that GW is Human Induced, by Party Identification")
dev.off()

png("2011_personalaction.png",width=600,height=400)
diffplot("PersonalAction","Willingness to Take Personal Action, by Party Identification")
dev.off()

png("2011_emissionscap.png",width=600,height=400)
diffplot("EmissionCap","Support for Emissions Cap, by Party Identification")
dev.off()


## BOOTSTRAP THE REGRESSION SEs (Table 2)
data.attentive <- subset(data,Condition %in% 1:4)
iter <- 3000
bootstrap.coefs <-
c(
"intercept","Norm_NoSci","NoNorm_Sci","Norm_Sci",#"NoNorm_PolSci","Norm_PolSci",
"partyid",#"partyid.log",
"partyid_Norm_NoSci","partyid_NoNorm_Sci","partyid_Norm_Sci"#,"partyid_NoNorm_PolSci","partyid_Norm_PolSci"
)

gwbelief.boot <- data.frame(matrix(nrow=iter,ncol=length(bootstrap.coefs))); names(gwbelief.boot) <- bootstrap.coefs
gwhuman.boot <- data.frame(matrix(nrow=iter,ncol=length(bootstrap.coefs))); names(gwhuman.boot) <- bootstrap.coefs
action.boot <- data.frame(matrix(nrow=iter,ncol=length(bootstrap.coefs))); names(action.boot) <- bootstrap.coefs
emission.boot <- data.frame(matrix(nrow=iter,ncol=length(bootstrap.coefs))); names(emission.boot) <- bootstrap.coefs
for (i in 1:iter){
    #Store samples w/ replacement in "temp"
    draws = sample(seq(1:dim(data.attentive)[1]),dim(data.attentive)[1],replace=TRUE)
    temp = data.attentive[draws,]
    #Calculate the coefs for this bootstrap iteration and store
    gwbelief.boot[i,] = as.vector(with(temp,lm(GWBelief~factor(Condition)*partyid))$coef)
    gwhuman.boot[i,] = as.vector(with(temp,lm(GWHuman~factor(Condition)*partyid))$coef)
    action.boot[i,] = as.vector(with(temp,lm(PersonalAction~factor(Condition)*partyid))$coef)
    emission.boot[i,] = as.vector(with(temp,lm(EmissionCap~factor(Condition)*partyid))$coef)
}

m = lapply(gwbelief.boot,mean); s = lapply(gwbelief.boot,sd); tab.gwbelief = cbind(as.numeric(m),as.numeric(s))
m = lapply(gwhuman.boot,mean); s = lapply(gwhuman.boot,sd); tab.gwhuman = cbind(as.numeric(m),as.numeric(s))
m = lapply(action.boot,mean); s = lapply(action.boot,sd); tab.action = cbind(as.numeric(m),as.numeric(s))
m = lapply(emission.boot,mean); s = lapply(emission.boot,sd); tab.emission = cbind(as.numeric(m),as.numeric(s))

colnames(tab.gwbelief) = 
colnames(tab.gwhuman) = 
colnames(tab.action) = 
colnames(tab.emission) = 
c("coef","se")

rownames(tab.gwbelief) = 
rownames(tab.gwhuman) = 
rownames(tab.action) = 
rownames(tab.emission) = 
bootstrap.coefs

round(tab.gwbelief,2)
round(tab.gwhuman,2)
round(tab.action,2)
round(tab.emission,2)





# APPENDIX: MODEL SPECIFICATIONS


modelcheck <- function(var){
    data$var <- data[,var]
    lm1 <- summary(with(data,lm(var~factor(Condition))))
    lm2 <- summary(with(data,lm(var~factor(Condition)*partyid)))
    lm3 <- summary(with(data,lm(var~factor(Condition)*partyid.bin)))
    lm4r <- summary(with(subset(data,partyid.bin==0),lm(var~factor(Condition))))
    lm4d <- summary(with(subset(data,partyid.bin==1),lm(var~factor(Condition))))
    lm5 <- summary(with(data,lm(var~factor(Condition)*knowledge)))
    tab <- cbind(
        c(coefpaste(lm1$coef[,1],lm1$coef[,2]),rep("-",5)),
        c(coefpaste(lm2$coef[,1],lm2$coef[,2]),"-"),
        c(coefpaste(lm3$coef[,1],lm3$coef[,2]),"-"),
        c(coefpaste(lm4r$coef[,1],lm4r$coef[,2]),rep("-",5)),
        c(coefpaste(lm4d$coef[,1],lm4d$coef[,2]),rep("-",5)),
        c(coefpaste(lm5$coef[1:4,1],lm5$coef[1:4,2]),rep("-",4),coefpaste(lm5$coef[5,1],lm5$coef[5,2]))
    )
    tab <- rbind(tab,round(c(lm1$sigma,lm2$sigma,lm3$sigma,lm4r$sigma,lm4d$sigma,lm5$sigma),2))
    colnames(tab) <- 1:6
    rownames(tab) <- c("Intercept","Norm Only","Science Only","Norm + Science",
                    "PartyId","PartyId * Norm Only","PartyId * Science Only","PartyId * Norm + Science","Knowledge","SER")
    print(tab,quote=FALSE)
    data$var <- NULL
}


modelcheck("GWBelief")
modelcheck("GWHuman")
modelcheck("PersonalAction")
modelcheck("EmissionCap")




# OTHER EXP RESULTS
with(data,expResults(Increase,Condition))
with(data,expResults(Decrease,Condition))
with(data,expResults(GWBelief,Condition))
with(data,expResults(GWHuman,Condition))
with(data,expResults(PersonalAction,Condition))
with(data,expResults(EmissionCap,Condition))
with(data,expResults(Email,Condition))
with(data,expResults(AdaptPrevent,Condition))

with(subset(data,partyid.bin==1),expResults(Increase,Condition))
with(subset(data,partyid.bin==1),expResults(Decrease,Condition))

with(subset(data,partyid.bin==1),expResults(GWHuman,Condition))
with(subset(data,partyid.bin==1),expResults(PersonalAction,Condition))
with(subset(data,partyid.bin==1),expResults(EmissionCap,Condition))
with(subset(data,partyid.bin==1),expResults(Email,Condition))
with(subset(data,partyid.bin==1),expResults(AdaptPrevent,Condition))

with(subset(data,partyid.bin==2),expResults(Increase,Condition))
with(subset(data,partyid.bin==2),expResults(Decrease,Condition))

with(subset(data,partyid.bin==2),expResults(GWHuman,Condition))
with(subset(data,partyid.bin==2),expResults(PersonalAction,Condition))
with(subset(data,partyid.bin==2),expResults(EmissionCap,Condition))
with(subset(data,partyid.bin==2),expResults(Email,Condition))
with(subset(data,partyid.bin==2),expResults(AdaptPrevent,Condition))

condlabs <- c("Control","Norm-Only","Science-Only","Norms & Science")
