#!bin/usr/R
# Created 2012-05-18
# Climate Change Experiment
# GSU 2010 Online Survey Experiment Data

# All variables precoded -1 to +1, with higher scores meaning agreement
# On human_induced, 1 means human caused, -1 means naturally caused
# On PartyID, -1 = Republican, 0 = Independent, +1 = Democrat

###############################################################################
## ANALYSIS
###############################################################################

data <- read.csv("gsu2010-data.csv")

library("car")
library("coin")
## code for treatment group dv means and SEs
.groupNames <- c("Control","ProNorm_Desc","ProNorm_DescInj","ConNorm_Desc","ConNorm_DescInj")
.norms <- c("Con Norm","No Norm","Pro Norm")


## RECODE PARTYID
data$partyid.tri <- recode(data$partyid, "-1:-.33=-1;0=0;.33:1=1")
data$partyid.bin <- recode(data$partyid.tri,"-1=0;0=NA;1=1") # indicator for rep (0), dem(+1)


## DESCRIPTIVES

round(prop.table(table(data$partyid.bin))*100,1)
round(prop.table(table(data$partyid.tri))*100,1)

with(data,coefpaste(mean(global_warming),sd(global_warming)))
with(data,coefpaste(mean(human_induced),sd(human_induced)))
with(data,coefpaste(mean(smallercar),sd(smallercar)))
with(data,coefpaste(mean(support_cap),sd(support_cap)))
with(data,coefpaste(mean(ce_ppi),sd(ce_ppi)))
with(data,coefpaste(mean(ce_er),sd(ce_er)))

data$norm <- recode(data$condition,"1=0;2:3=1;4:5=-1")
data$connorm <- recode(data$condition,"1=0;2:3=0;4:5=1")

with(data,expResults(global_warming,norm,.norms))
with(data,expResults(human_induced,norm,.norms))
with(data,expResults(smallercar,norm,.norms))
with(data,expResults(support_cap,norm,.norms))
with(data,expResults(ce_ppi,norm,.norms))
with(data,expResults(ce_er,norm,.norms))

## TESTS OF CON NORMS
with(data,independence_test(global_warming~connorm,alternative="less"))
with(data,independence_test(human_induced~connorm,alternative="less"))
with(data,independence_test(smallercar~connorm,alternative="less"))
with(data,independence_test(support_cap~connorm,alternative="less"))
with(data,independence_test(ce_ppi~connorm,alternative="less"))
with(data,independence_test(ce_er~connorm,alternative="less"))


partydiff <- function(var){
    data$var <- data[,var]
    x1 <- with(subset(data,partyid.bin==0),expResults(var,condition))
    x2 <- with(subset(data,partyid.bin==1),expResults(var,condition))
    x3m <- x2[,1]-x1[,1]
    x3s <- sqrt(x2[,4]+x1[,4])
    tab <- cbind(coefpaste(x1[,1],x1[,4]),coefpaste(x2[,1],x2[,4]),coefpaste(x3m,x3s))
    data$var <- NULL
    colnames(tab) <- c("Republicans","Democrats","Difference (D-R)")
    #print(tab,quote=FALSE)
    return(list(x1[,c(1,4)],x2[,c(1,4)],cbind(x3m,x3s)))
}

gwbelief <- partydiff("global_warming")
gwhuman <- partydiff("human_induced")
car <- partydiff("smallercar")
emissions <- partydiff("support_cap")
ppi <- partydiff("ce_ppi")
er <- partydiff("ce_er")

# PLOTS

diffplot <- function(var,title) {
    obj <- partydiff(var)
    plot((1:5)-.1,obj[[1]][,1], pch=15, cex=1.5, xaxt="n", yaxt="n", xlab="", ylab="", bty="n",
        main=title, xlim=c(.5,5.5), ylim=c(-.4,.8)) # Plot Republicans
    axis(1,1:5,c("Control", "Pro Desc", "Pro Desc + Inj", "Con Desc", "Con Desc + Inj"),cex=.5) # X-axis
    axis(2,las=1) # Y-axis
    points((1:5)+.1,obj[[2]][,1], pch=19, cex=1.7) # Plot Democrats
    for(i in 1:5){
        segments(i-.1,obj[[1]][i,1]-obj[[1]][i,2],i-.1,obj[[1]][i,1]+obj[[1]][i,2],lwd=2) # Plot Republican SEs
        segments(i+.1,obj[[2]][i,1]-obj[[2]][i,2],i+.1,obj[[2]][i,1]+obj[[2]][i,2],lwd=2) # Plot Democratic SEs
    }
    text(1,obj[[1]][1,1],"Reps",pos=4)
    text(1.2,obj[[2]][1,1],"Dems",pos=4)
}

png("2010_gwbelief.png",width=700,height=400)
diffplot("global_warming","Belief in Global Warming, by Party Identification")
dev.off()

png("2010_gwhuman.png",width=700,height=400)
diffplot("human_induced","Belief that GW is Human Induced, by Party Identification")
dev.off()

png("2010_personalaction.png",width=700,height=400)
diffplot("smallercar","Willingness to Take Personal Action, by Party Identification")
dev.off()

png("2010_emissionscap.png",width=700,height=400)
diffplot("support_cap","Support for Emissions Cap, by Party Identification")
dev.off()

png("2010_ppi.png",width=700,height=400)
diffplot("ce_ppi","Perceived Personal Influence on GW, by Party Identification")
dev.off()

png("2010_er.png",width=700,height=400)
diffplot("ce_er","Expected Reciprocity, by Party Identification")
dev.off()


## BOOTSTRAP THE REGRESSION SEs (Table 1)
iter = 3000
bootstrap.coefs <-
c(
"intercept","ProNorm_Desc","ProNorm_DescInj","ConNorm_Desc","ConNorm_DescInj",
"partyid",
"partyid_ProNorm_Desc","partyid_ProNorm_DescInj","partyid_ConNorm_Desc","partyid_ConNorm_DescInj"
)
gwbelief.boot <- data.frame(matrix(nrow=iter,ncol=length(bootstrap.coefs))); names(gwbelief.boot) <- bootstrap.coefs
gwhuman.boot <- data.frame(matrix(nrow=iter,ncol=length(bootstrap.coefs))); names(gwhuman.boot) <- bootstrap.coefs
action.boot <- data.frame(matrix(nrow=iter,ncol=length(bootstrap.coefs))); names(action.boot) <- bootstrap.coefs
emission.boot <- data.frame(matrix(nrow=iter,ncol=length(bootstrap.coefs))); names(emission.boot) <- bootstrap.coefs
personal.boot <- data.frame(matrix(nrow=iter,ncol=length(bootstrap.coefs))); names(personal.boot) <- bootstrap.coefs
recip.boot <- data.frame(matrix(nrow=iter,ncol=length(bootstrap.coefs))); names(recip.boot) <- bootstrap.coefs
for (i in 1:iter){
    #Store samples w/ replacement in "temp"
    draws = sample(seq(1:dim(data)[1]),dim(data)[1],replace=TRUE)
    temp = data[draws,]
    #Calculate the coefs for this bootstrap iteration and store
    gwbelief.boot[i,] = as.vector(with(temp,lm(global_warming~factor(condition)*partyid))$coef)
    gwhuman.boot[i,] = as.vector(with(temp,lm(human_induced~factor(condition)*partyid))$coef)
    action.boot[i,] = as.vector(with(temp,lm(smallercar~factor(condition)*partyid))$coef)
    emission.boot[i,] = as.vector(with(temp,lm(support_cap~factor(condition)*partyid))$coef)
    personal.boot[i,] = as.vector(with(temp,lm(ce_ppi~factor(condition)*partyid))$coef)
    recip.boot[i,] = as.vector(with(temp,lm(ce_er~factor(condition)*partyid))$coef)
}

m = lapply(gwbelief.boot,mean); s = lapply(gwbelief.boot,sd); tab.gwbelief = cbind(as.numeric(m),as.numeric(s))
m = lapply(gwhuman.boot,mean); s = lapply(gwhuman.boot,sd); tab.gwhuman = cbind(as.numeric(m),as.numeric(s))
m = lapply(action.boot,mean); s = lapply(action.boot,sd); tab.action = cbind(as.numeric(m),as.numeric(s))
m = lapply(emission.boot,mean); s = lapply(emission.boot,sd); tab.emission = cbind(as.numeric(m),as.numeric(s))
m = lapply(personal.boot,mean); s = lapply(personal.boot,sd); tab.personal = cbind(as.numeric(m),as.numeric(s))
m = lapply(recip.boot,mean); s = lapply(recip.boot,sd); tab.recip = cbind(as.numeric(m),as.numeric(s))

colnames(tab.gwbelief) = 
colnames(tab.gwhuman) = 
colnames(tab.action) = 
colnames(tab.emission) = 
colnames(tab.personal) = 
colnames(tab.recip) = 
c("coef","se")

rownames(tab.gwbelief) = 
rownames(tab.gwhuman) = 
rownames(tab.action) = 
rownames(tab.emission) = 
rownames(tab.personal) = 
rownames(tab.recip) = 
bootstrap.coefs

round(tab.gwbelief,2)
round(tab.gwhuman,2)
round(tab.action,2)
round(tab.emission,2)
round(tab.personal,2)
round(tab.recip,2)





## APPENDIX: MODEL SPECIFICATIONS

modelcheck <- function(var){
    data$var <- data[,var]
    lm1 <- summary(with(data,lm(var~factor(condition))))
    lm2 <- summary(with(data,lm(var~factor(condition)*partyid)))
    lm3 <- summary(with(data,lm(var~factor(condition)*partyid.bin)))
    lm4r <- summary(with(subset(data,partyid.bin==0),lm(var~factor(condition))))
    lm4d <- summary(with(subset(data,partyid.bin==1),lm(var~factor(condition))))
    #lm5 <- summary(with(data,lm(var~factor(condition)*knowledge)))
    tab <- cbind(
        c(coefpaste(lm1$coef[,1],lm1$coef[,2]),rep("-",5)),
        coefpaste(lm2$coef[,1],lm2$coef[,2]),
        coefpaste(lm3$coef[,1],lm3$coef[,2]),
        c(coefpaste(lm4r$coef[,1],lm4r$coef[,2]),rep("-",5)),
        c(coefpaste(lm4d$coef[,1],lm4d$coef[,2]),rep("-",5))
        #coefpaste(lm5$coef[,1],lm5$coef[,2])
    )
    tab <- rbind(tab,round(c(lm1$sigma,lm2$sigma,lm3$sigma,lm4r$sigma,lm4d$sigma),2))#,lm5$sigma),2))
    colnames(tab) <- 1:5
    rownames(tab) <- c("Intercept","Pro Desc","Pro Desc + Inj","Con Desc","Con Desc + Inj",
                    "PartyId","PartyId * Pro Desc","PartyId * Pro Desc + Inj","PartyId * Con Desc","PartyId * Con Desc + nj","SER")
    print(tab,quote=FALSE)
    data$var <- NULL
}

modelcheck("ce_ppi")
modelcheck("ce_er")
modelcheck("global_warming")
modelcheck("human_induced")
modelcheck("support_cap")
modelcheck("smallercar")


