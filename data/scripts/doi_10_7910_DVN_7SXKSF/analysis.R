################################################################################
################################################################################
#####################Dissertation Chapter 1 Analysis############################
################################################################################
################################################################################

#setwd("C:\\Users\\THOMAS\\Google Drive\\Dissertation\\analysis 1 - attitude strength and info seeking\\drafts")

library("foreign")   # v0.8
library("car")       # v2.1-1
library("coin")      # v1.1-2
library("xtable")    # v1.8-0

# my code
source("functions.R")
# condition labels
.groupNames <- condlab1 <- c("Control","High Choice Pro","High Choice Con","High Captive Pro","High Captive Con","Low Choice Pro","Low Choice Con","Low Captive Pro","Low Captive Con")
.groupNames <- condlab2 <- c("High Choice Pro","High Choice Con","High Captive Pro","High Captive Con","Low Choice Pro","Low Choice Con","Low Captive Pro","Low Captive Con")

data <- read.dta("HybridAttImpData.dta")

################################################################################
### VARIABLE RECODING ##########################################################
################################################################################
#data$age #Age
data$female=data$female2 #Female
data$minority=data$minority1 #Non-white=1
data$income=round(data$income/5,2) #Rescaled 0-1
data$edu.highschool=recode(data$education,"1=0;2=1;3=1;4=1;5=1") #high school graduate
data$edu.college=recode(data$education,"1=0;2=0;3=1;4=1;5=1") #any college
data$discuss=data$talkpoli-1 #days per week
data$tv=data$tvnews-1 #days per week
data$newspaper=data$newspaper-1 #days per week
data$online=data$onlineinfo-1 #days per week
data$partyid=recode(data$pidrep,"1=1;2=.67;3=.33;4=0;5=-.33;6=-.67;7=-1") #-1=Republican, 1=Democrat
data$trust=round(data$trustgov/4,2) #trust scaled 0-1
data$knowledge=data$pknow #scaled 0-1

################################################################################

#table(data$rencondit) #randomized conditions (ignoring choices for nonrandom groups)
data$conditions=recode(data$rencondit,"1=0;2:5=1;6=3;7=4;8:11=5;12=7;13=8")
data$choice.imp=recode(data$conditions,"1=1;5=0;else=NA")
data$rand.imp=recode(data$conditions,"1=1;3=1;4=1;5=0;7=0;8=0;0=NA")
data$rand.msg=recode(data$conditions,"3=1;4=-1;7=1;8=-1;0=0;else=NA")
data$prior=round((data$rstelec-1)/6,2)
data$prior.bin=recode(data$prior,"lo:.49=-1;.5=0;.51:hi=1")
data$prior.con=recode(data$prior,"lo:.49=1;else=0")
data$prior.pro=recode(data$prior,".51:hi=1;else=0")
data$prior.neutral=recode(data$prior,".5=1;else=0")
data=subset(data,!is.na(data$prior))

data$choicehi=recode(data$br1_4read,"1=1;2=0") #Choice among high importance, 1=beneficial (pro), 2=ineffective (con)
data$choicelo=recode(data$br7_10read,"1=1;2=0") #Choice among low importance, 1=beneficial (pro), 2=ineffective (con)

#choice=recode(choicehi,"NA=0")+recode(choicelo,"NA=0")
data$choice=NA
for (i in 1:length(data$choicehi)){
    if(data$conditions[i]==5)
        data$choice[i]=data$choicelo[i]
    if(data$conditions[i]==1)
        data$choice[i]=data$choicehi[i]
}

data$choice.inv=recode(data$choice,"1=0;0=1")
cong.pro=data$prior.pro*data$choice
cong.con=data$prior.con*data$choice.inv

# Choice conditions: 1 pro, 2 con (high imp); 5 pro, 6 con (low imp)
## could also produce conditions2 by setting NAs in choice.inv=0 and summing conditions+choice.inv
data$conditions2=data$conditions #Build treatment groups (including nonrandom treatment groups)
data$conditions.choice=data$conditions+data$choice.inv
for (i in 1:length(data$conditions)){
    if(data$conditions[i]==1) data$conditions2[i]=data$conditions[i]+data$choice.inv[i]
    if(data$conditions[i]==5) data$conditions2[i]=data$conditions[i]+data$choice.inv[i]
}

for(i in 1:length(cong.pro)){
    # calculate congruence scores among choosers
    if (data$conditions2[i]==1 & data$prior.pro[i]==1) data$cong.choice[i]=1
    if (data$conditions2[i]==1 & data$prior.con[i]==1) data$cong.choice[i]=0
    if (data$conditions2[i]==2 & data$prior.con[i]==1) data$cong.choice[i]=1
    if (data$conditions2[i]==2 & data$prior.pro[i]==1) data$cong.choice[i]=0
    if (data$conditions2[i]==5 & data$prior.pro[i]==1) data$cong.choice[i]=1
    if (data$conditions2[i]==5 & data$prior.con[i]==1) data$cong.choice[i]=0
    if (data$conditions2[i]==6 & data$prior.con[i]==1) data$cong.choice[i]=1
    if (data$conditions2[i]==6 & data$prior.pro[i]==1) data$cong.choice[i]=0
}

for (i in 1:length(data$conditions2)){
    # calculate congruence scores for everyone
    if (data$conditions2[i]==0) data$congruent[i]=NA
    if (data$prior[i]==.5) data$congruent[i]=NA
    if (data$conditions2[i]==1 & data$prior.pro[i]==1) data$congruent[i]=1
    if (data$conditions2[i]==1 & data$prior.con[i]==1) data$congruent[i]=0
    if (data$conditions2[i]==2 & data$prior.con[i]==1) data$congruent[i]=1
    if (data$conditions2[i]==2 & data$prior.pro[i]==1) data$congruent[i]=0
    if (data$conditions2[i]==3 & data$prior.pro[i]==1) data$congruent[i]=1
    if (data$conditions2[i]==3 & data$prior.con[i]==1) data$congruent[i]=0
    if (data$conditions2[i]==4 & data$prior.con[i]==1) data$congruent[i]=1
    if (data$conditions2[i]==4 & data$prior.pro[i]==1) data$congruent[i]=0
    if (data$conditions2[i]==5 & data$prior.pro[i]==1) data$congruent[i]=1
    if (data$conditions2[i]==5 & data$prior.con[i]==1) data$congruent[i]=0
    if (data$conditions2[i]==6 & data$prior.con[i]==1) data$congruent[i]=1
    if (data$conditions2[i]==6 & data$prior.pro[i]==1) data$congruent[i]=0
    if (data$conditions2[i]==7 & data$prior.pro[i]==1) data$congruent[i]=1
    if (data$conditions2[i]==7 & data$prior.con[i]==1) data$congruent[i]=0
    if (data$conditions2[i]==8 & data$prior.con[i]==1) data$congruent[i]=1
    if (data$conditions2[i]==8 & data$prior.pro[i]==1) data$congruent[i]=0
}

data$cond4 <- recode(data$conditions, "0=NA; c(1,2)=1; c(3,4)=2; c(5,6)=3; c(7,8)=4")

data$msg=recode(data$conditions2,"0=0;1=1;2=-1;3=1;4=-1;5=1;6=-1;7=1;8=-1") #Pro=1, Con=-1, Control=0
data$msg.pro=recode(data$msg,"0=0;-1=0;1=1") #Pro=1, Con=0, Control=0
data$msg.con=recode(data$msg,"0=0;1=0;-1=1") #Pro=0, Con=1, Control=0
data$captive=recode(data$conditions2,"0=NA;c(1,2,5,6)=0;else=1")

data$dv=round((data$buyren-1)/6,2) #DV Higher scores more supportive (scaled 0 to 1)
data$change.dv=data$dv-data$prior #change in the dv T2-T1 (Scaled -1 to 1)
data$change.abs=abs(data$change.dv) #abs value of change (Scaled 0 to 1)
data$extreme1=abs(data$prior-.5)
data$extreme2=abs(data$dv-.5)
data$extreme.chg=with(data,extreme2-extreme1)
data$attimp=round((data$brimp-1)/6,2) #attitude importance (Scaled 0 to 1)
data$effective=round((data$breff-1)/6,2) #argument effectiveness; not asked of control group (Scaled 0 to 1)
data$info=round((data$brinfo-1)/6,2) #information-seeking (Scaled 0 to 1)
data$email=recode(data$bremail,"1=1;2=0") #recoded from: 1=yes, 2=no



################################################################################
### ANALYSIS ###################################################################
################################################################################

#--------------#
# DEMOGRAPHICS #
#--------------#

median(data$age)
sprintf("%0.1f", 100*prop.table(table(data$female))[2]) # female
sprintf("%0.1f", 100*prop.table(table(data$minority))[1]) # white
sprintf("%0.1f", 100*prop.table(table(data$edu.highschool))[2]) # at least high school
sprintf("%0.1f", 100*prop.table(table(data$edu.college))[2]) # at least college
sprintf("%0.1f", 100*prop.table(table(data$partyid > 0))) # Democrats
sprintf("%0.1f", 100*prop.table(table(data$partyid < 0))) # Republicans

#--------------------#
# MANIPULATION CHECK #
#--------------------#

# CHECK THAT IMPORTANCE MANIPULATION IMPACTED IMPORTANCE MEASURE
#round(with(data, expResults(attimp,conditions2,"imp.means2")),2)
cbind(round(by(data$attimp,data$rand.imp,FUN=mean),2),
      round(by(data$attimp,data$rand.imp,FUN=sd),2),
      by(data$attimp,data$rand.imp,FUN=length)) #dv by importance group
with(data,independence_test(attimp~rand.imp)) # high imp compared to low imp
hitemp <- recode(data$rand.imp,"1=1;0=NA;NA=0") # temporary comparison between high imp and control for stat sig test
lotemp <- recode(data$rand.imp,"1=NA;0=1;NA=0") # temporary comparison between high imp and control for stat sig test
with(data,independence_test(attimp~hitemp)) # high imp compared to control
with(data,independence_test(attimp~lotemp)) # low imp compared to control
# CHECK THAT IMPORTANCE DID NOT AFFECT DVs
with(data,independence_test(effective~rand.imp)) # not significant
with(data,independence_test(dv~rand.imp)) # not significant
with(data,independence_test(change.dv~rand.imp)) # not significant
with(data,independence_test(info~rand.imp)) # significant, as expected
with(data,independence_test(email~rand.imp)) # significant, as expected


#---------------------#
# INFORMATION CHOICES #
#---------------------#

# Proportions choosing pro/con and congruent/incongruent under high and low importance
round(prop.table(with(subset(data,data$conditions==1),table(conditions,congruent)),1),2) #Proportion making congruent choice (high imp)
round(prop.table(with(subset(data,data$conditions==5),table(conditions,congruent)),1),2) #Proportion making congruent choice (low imp)
# break those results out by prior opinion
round(prop.table(with(subset(data,data$conditions==1 & prior.bin==-1),table(conditions,choice)),1),2)
round(prop.table(with(subset(data,data$conditions==1 & prior.bin==1),table(conditions,choice)),1),2)
round(prop.table(with(subset(data,data$conditions==5 & prior.bin==-1),table(conditions,choice)),1),2)
round(prop.table(with(subset(data,data$conditions==5 & prior.bin==1),table(conditions,choice)),1),2)

# Choice odds ratio (making attitude-congruent choice)
local({
    choice.oddslo=round((with(data,table(congruent,choice.imp))[2,1])/(with(data,table(congruent,choice.imp))[1,1]),2)
    choice.oddshi=round((with(data,table(congruent,choice.imp))[2,2])/(with(data,table(congruent,choice.imp))[1,2]),2)
    #choice.oddslo #roughly equally likely to make congruent choice
    #choice.oddshi #Much more likely to make congruent choice under high att imp
    choice.or=round(choice.oddshi/choice.oddslo,2)
    #choice.or #40% more likely to choose congruently under hi than lo importance
    log.choice=log(choice.or) #sig test
    se.log.choice=sqrt((1/with(data,table(congruent,choice.imp))[1,1])+(1/with(data,table(congruent,choice.imp))[2,1])+(1/with(data,table(congruent,choice.imp))[1,2])+(1/with(data,table(congruent,choice.imp))[2,2])) #se for log-odds
    choice.or.ci95lo=round(exp(log.choice-1.959*se.log.choice),2) #CI in log terms
    choice.or.ci95hi=round(exp(log.choice+1.959*se.log.choice),2)
    choice.or.ci90lo=round(exp(log.choice-1.644*se.log.choice),2) #CI in log terms
    choice.or.ci90hi=round(exp(log.choice+1.644*se.log.choice),2)
    #Odds-Ratio Results
    cat("Odds (High Importance): ",choice.oddshi,"\n",
        "Odds (Low Importance): ",choice.oddslo,"\n",
        "Odds-Ratio: ",choice.or," (",choice.or.ci95lo,",",choice.or.ci95hi,")"," (",choice.or.ci90lo,",",choice.or.ci90hi,")","\n",sep="")
})



#---------------------------------#
# GAINES AND KUKLINSKI ESTIMATORS #
#---------------------------------#

gkest <- function(dv, d = data) {
    ## 5000 bootstrap estimations
    b <- 5000
    
    ## estimators
    meandiff <- function(a, b, den = 1) {
        unname((mean(a, na.rm = TRUE) - mean(b, na.rm = TRUE))/den)
    }
    teest <- function(y1, y0) {
        m <- meandiff(y1, y0)
        sampg <- c(rep(0, length(y0)), rep(1, length(y1)))
        s <- replicate(b, {
            sampy <- sample(c(y1, y0), length(c(y1,y0)), TRUE)
            meandiff(sampy[sampg == 0], sampy[sampg == 1])
        })
        paste0(sprintf("%0.2f", m), " (", sprintf("%0.2f", sd(s, na.rm = TRUE)), ")")
    }
    gkts <- function(ys, yc, alpha) {
        m <- meandiff(ys, yc, den = alpha)
        sampg <- c(rep(0, length(ys)), rep(1, length(yc)))
        s <- replicate(b, {
            sampy <- sample(c(ys, yc), length(c(ys,yc)), TRUE)
            meandiff(sampy[sampg == 0], sampy[sampg == 1], den = alpha)
        })
        paste0(sprintf("%0.2f", m), " (", sprintf("%0.2f", sd(s, na.rm = TRUE)), ")")
    }
    gktn <- function(yt, ys, alpha) {
        m <- meandiff(yt, ys, den = 1-alpha)
        sampg <- c(rep(0, length(yt)), rep(1, length(ys)))
        s <- replicate(b, {
            sampy <- sample(c(yt, ys), length(c(yt,ys)), TRUE)
            meandiff(sampy[sampg == 0], sampy[sampg == 1], den = 1-alpha)
        })
        paste0(sprintf("%0.2f", m), " (", sprintf("%0.2f", sd(s, na.rm = TRUE)), ")")
    }
    
    d <- d[d$msg != 0,]
    dcap <- d[d$captive == 1,]
    dlo <- d[d$rand.imp == 0,]
    dhi <- d[d$rand.imp == 1,]
    
    ## RANDOMIZED TREATMENT EFFECT
    te <- teest(dcap[dcap$rand.msg == 1, dv], 
                dcap[dcap$rand.msg == -1, dv])
    te_lo <- teest(dcap[dcap$rand.msg == 1 & dcap$rand.imp == 0, dv], 
                   dcap[dcap$rand.msg == -1 & dcap$rand.imp == 0, dv])
    te_hi <- teest(dcap[dcap$rand.msg == 1 & dcap$rand.imp == 1, dv], 
                   dcap[dcap$rand.msg == -1 & dcap$rand.imp == 1, dv])
    
    ## Gaines/Kuklinski ESTIMATORS
    ### IGNORING HI/LO MANIPULATION
    ts <- gkts(ys = d[d$captive == 0, dv], 
               yc = d[d$captive == 1 & d$msg == -1, dv], 
               alpha = prop.table(table(d$choice))[2])
    tn <- gktn(yt = d[d$captive == 1 & d$msg == 1, dv], 
               ys = d[d$captive == 0, dv], 
               alpha = prop.table(table(d$choice))[2])
    ### LOW IMPORTANCE
    ts_lo <- gkts(ys = dlo[dlo$captive == 0, dv], 
                  yc = dlo[dlo$captive == 1 & dlo$msg == -1, dv], 
                  alpha = prop.table(table(dlo$choice))[2])
    tn_lo <- gktn(yt = dlo[dlo$captive == 1 & dlo$msg == 1, dv], 
                  ys = dlo[dlo$captive == 0, dv], 
                  alpha = prop.table(table(dlo$choice))[2])
    ### HIGH IMPORTANCE
    ts_hi <- gkts(ys = dhi[dhi$captive == 0, dv], 
                  yc = dhi[dhi$captive == 1 & dhi$msg == -1, dv], 
                  alpha = prop.table(table(dhi$choice))[2])
    tn_hi <- gktn(yt = dhi[dhi$captive == 1 & dhi$msg == 1, dv], 
                  ys = dhi[dhi$captive == 0, dv], 
                  alpha = prop.table(table(dhi$choice))[2])
    c(te = te, ts = ts, tn = tn, 
      te_lo = te_lo, ts_lo = ts_lo, tn_lo = tn_lo, 
      te_hi = te_hi, ts_hi = ts_hi, tn_hi = tn_hi)
}

set.seed(61594)
gktab <- rbind(
    gkest("effective", d = data), # ARGUMENT EVALUATION
    gkest("dv", d = data),        # OPINION
    gkest("change.dv", d = data), # OPINION CHANGE
    gkest("info", d = data),      # INFORMATION-SEEKING (SUBJECTIVE)
    gkest("email", d = data)      # INFORMATION-SEEKING (EMAIL)
)
colnames(gktab) <- rep(c("SATE", "PSE", "CSE"), 3)
rownames(gktab) <- c("Evaluation", "Opinion", "Opinion Change ($t2-t1$)", "Information-Seeking", "Email Request")
print(xtable(gktab[,1:3], align = c("l", rep("r", 3))), 
      sanitize.colnames.function = I, sanitize.rownames.function = I, file = "tables/results1.tex", floating = FALSE)
print(xtable(gktab[,4:6], align = c("l", rep("r", 3))), 
      sanitize.colnames.function = I, sanitize.rownames.function = I, file = "tables/results2.tex", floating = FALSE)
print(xtable(gktab[,7:9], align = c("l", rep("r", 3))), 
      sanitize.colnames.function = I, sanitize.rownames.function = I, file = "tables/results3.tex", floating = FALSE)



#------------------------------------#
# TREATMENT GROUP MEANS FOR APPENDIX #
#------------------------------------#

# EVALUATIONS
local({
    x <- with(data, expResults(effective,conditions2,condlab1))
    x <- cbind(coefpaste(x[,1], x[,4]), x[,3])[-1,]
    colnames(x) <- c("Mean (SE)", "N")
    print(xtable(x, align = c("l", "r", "r")), 
          floating = FALSE, file = "tables/app-evaltab1.tex")

    x <- with(data, expResults(effective,cond4,c("Choice High","Captive High","Choice Low","Captive Low")))
    x <- cbind(coefpaste(x[,1], x[,4]), x[,3])
    colnames(x) <- c("Mean (SE)", "N")
    print(xtable(x, align = c("l", "r", "r")), 
          floating = FALSE, file = "tables/app-evaltab2.tex")

    # TREATMENT GROUP (COLLAPSED) EFFECTIVENESS RATING MEANS (BY CONGRUENCE)
    eff.all <- with(data, expResults(effective,cond4,c("Choice High","Captive High","Choice Low","Captive Low")))

    # treatment group effectiveness means (by prior)
    eff.means2 <- with(data, expResults(effective,conditions2,condlab1))
    eff.con <- with(subset(data,prior.bin==-1), expResults(effective,conditions2,condlab1))
    eff.pro <- with(subset(data,prior.bin==1), expResults(effective,conditions2,condlab1))
    # calculate evaluations for each group (congruence x choice)
    eff.incong <- with(subset(data,congruent==0), expResults(effective,cond4,c("Choice High","Captive High","Choice Low","Captive Low")))
    eff.cong <- with(subset(data,congruent==1), expResults(effective,cond4,c("Choice High","Captive High","Choice Low","Captive Low")))
    # use mean evaluations to calculate evaluative bias
    bias.mean <- eff.cong[,1] - eff.incong[,1]
    bias.se <- sqrt( ((eff.cong[,2]^2)/eff.cong[,3])+((eff.incong[,2]^2)/eff.incong[,3]) )
    efftab2 <- 
    cbind(coefpaste(eff.incong[1:4,1], eff.incong[1:4,4]),
          coefpaste(eff.cong[1:4,1], eff.cong[1:4,4]),
          coefpaste(bias.mean, bias.se))
    colnames(efftab2) <- c("Incongruent","Congruent","Evaluative Bias")
    print(xtable(efftab2, caption="Mean Argument Evaluations by Treatment Group", label="tab:effective"), file = "tables/app-evalbias.tex")

    # PLOT EFFECTIVENESS RATINGS BY CONGRUENCE (eval bias is due to more negative ratings of incongruent messages)
    pdf("figures/evallevels.pdf", width = 7.5, height = 3)
    par(mar = c(4,6,1,1))
    errors1 <- rbind(
                cbind(eff.incong[,1]-(eff.incong[,4]), eff.incong[,1]+(eff.incong[,4])),
                cbind(eff.cong[,1]-(eff.cong[,4]), eff.cong[,1]+(eff.cong[,4]))
              )
    errors2 <- rbind(
                cbind(eff.incong[,1]-(2*eff.incong[,4]), eff.incong[,1]+(2*eff.incong[,4])),
                cbind(eff.cong[,1]-(2*eff.cong[,4]), eff.cong[,1]+(2*eff.cong[,4]))
              )
    xpos <- sort(c((1:4)-.2,c(1:4+.2)))
    ypos <- c(1,5,2,6,3,7,4,8)
    plot(c(eff.incong[,1], eff.cong[,1])[ypos], xpos, ylim=c(0.5,4.5), xlim=c(0,1), yaxs = "i", xaxs = "i", yaxt="n", ylab="", bty="n", xlab="", pch=rep(c(15,16)))
    axis(2, 1:4, c(rownames(efftab2)), las = 2)
    segments(errors1[ypos,1], xpos, errors1[ypos,2], xpos, lwd = 2)
    segments(errors2[ypos,1], xpos, errors2[ypos,2], xpos, lwd = 1)
    legend("topright", c("Incongruent", "Congruent"), pch = c(15,16), bty = "n")
    dev.off()
    
    pdf("figures/evalbias.pdf", width = 7.5, height = 3)
    par(mar = c(4,6,1,1))
    plot(bias.mean, seq_along(bias.mean), pch=23, col="black", bg="black", xlim=c(0.0,0.8), ylim=c(0.5,4.5), xlab="Evaluative Bias", ylab="", bty="l", yaxt="n")
    axis(2,1:4,c("Choice High","Captive High","Choice Low","Captive Low"), las = 2)
    segments(bias.mean-bias.se,seq_along(bias.mean),bias.mean+bias.se,seq_along(bias.mean),lwd=2)
    segments(bias.mean-(2*bias.se),seq_along(bias.mean),bias.mean+(2*bias.se),seq_along(bias.mean),lwd=1)
    dev.off()

    
})

# OPINION AND OPINION CHANGES
local({
    xt1 <- with(data, expResults(prior,conditions2,condlab1))
    xt2 <- with(data, expResults(dv,conditions2,condlab1))
    xchange <- with(data, expResults(change.dv,conditions2,condlab1))
    x <- cbind(coefpaste(xt1[,1], xt1[,4]), coefpaste(xt2[,1], xt2[,4]),coefpaste(xchange[,1], xchange[,4]),xchange[,3])
    colnames(x) <- c("t1 Mean (SE)", "t2 Mean (SE)", "t2-t1 Mean (SE)", "N")
    print(xtable(x, align = c("l", rep("r", 4))), 
          floating = FALSE, file = "tables/app-opiniontab1.tex")
    
    xt1 <- with(data, expResults(prior,cond4,c("Choice High","Captive High","Choice Low","Captive Low")))
    xt2 <- with(data, expResults(dv,cond4,c("Choice High","Captive High","Choice Low","Captive Low")))
    xchange <- with(data, expResults(change.dv,cond4,c("Choice High","Captive High","Choice Low","Captive Low")))
    x <- cbind(coefpaste(xt1[,1], xt1[,4]), coefpaste(xt2[,1], xt2[,4]),coefpaste(xchange[,1], xchange[,4]),xchange[,3])
    colnames(x) <- c("t1 Mean (SE)", "t2 Mean (SE)", "t2-t1 Mean (SE)", "N")
    print(xtable(x, align = c("l", rep("r", 4))), 
          floating = FALSE, file = "tables/app-opiniontab2.tex")
})


# INFORMATION-SEEKING
local({
    # DESCRIPTIVES FOR HIGH VS. LOW IMPORTANCE (BOTH MEASURES)
    with(data, expResults(email,rand.imp,c("Low","High")))
    with(data, expResults(info,rand.imp,c("Low","High")))

    # DESCRIPTIVES FOR MODERATING EFFECT OF CHOICE
    emailtab <- with(data, expResults(email,conditions2,condlab1))
    subjtab <- with(data, expResults(info,conditions2,condlab1))
    infotab <- cbind(coefpaste(subjtab[,1],subjtab[,4]),
                     coefpaste(emailtab[,1],emailtab[,4]), emailtab[,3])
    colnames(infotab) <- c("Subjective","Email","N")
    print(xtable(infotab, align = c("l", "r", "r", "r")), file = "tables/app-infotab1.tex")
    
    emailtab <- with(data, expResults(email,cond4,c("Choice High","Captive High","Choice Low","Captive Low")))
    subjtab <- with(data, expResults(info,cond4,c("Choice High","Captive High","Choice Low","Captive Low")))
    infotab <- cbind(coefpaste(subjtab[,1],subjtab[,4]),
                     coefpaste(emailtab[,1],emailtab[,4]), emailtab[,3])
    colnames(infotab) <- c("Subjective","Email","N")
    print(xtable(infotab, align = c("l", "r", "r", "r")), file = "tables/app-infotab2.tex")

    # PERMUTATION TEST FOR EFFECT OF IMPORTANCE ON SUBJECTIVE MEASURE
    with(data,independence_test(info~rand.imp)) # high imp compared to low imp
    hitemp <- recode(data$rand.imp,"1=1;0=NA;NA=0") # temporary comparison between high imp and control for stat sig test
    lotemp <- recode(data$rand.imp,"1=NA;0=1;NA=0") # temporary comparison between high imp and control for stat sig test
    with(data,independence_test(info~hitemp)) # high imp compared to control
    with(data,independence_test(info~lotemp)) # low imp compared to control

    ## PERMUTATION TEST FOR EFFECT OF IMPORTANCE ON EMAIL MEASURE
    with(data,independence_test(email~rand.imp)) # high imp compared to low imp
    with(data,independence_test(email~hitemp)) # high imp compared to control
    with(data,independence_test(email~lotemp)) # low imp compared to control

    # ODDS-RATIO TESTS FOR EFFECT OF IMPORTANCE ON EMAIL MEASURE
    emailtab <- with(data,table(email,rand.imp))
    email.oddslo=emailtab[2,1]/(emailtab[1,1]+emailtab[2,1])
    email.oddshi=emailtab[2,2]/(emailtab[1,2]+emailtab[2,2])
    email.oddslo
    email.oddshi
    email.or=round(email.oddshi/email.oddslo,2)
    email.or #much more likely to want to seek additional information under high attitude imp
    ##sig test
    log.email=log(email.or)
    se.log.email=sqrt((1/emailtab[1,1])+(1/emailtab[2,1])+(1/emailtab[1,2])+(1/emailtab[2,2])) #se for log-odds
    email.or.ci95lo=round(exp(log.email-1.959*se.log.email),2) #CI in log terms
    email.or.ci95hi=round(exp(log.email+1.959*se.log.email),2)
    email.or.ci90lo=round(exp(log.email-1.644*se.log.email),2) #CI in log terms
    email.or.ci90hi=round(exp(log.email+1.644*se.log.email),2)
    #Odds-Ratio Results
    cat("Odds (High Importance): ",round(email.oddshi,2),"\n",
        "Odds (Low Importance): ",round(email.oddslo,2),"\n",
        "Odds-Ratio: ",email.or," (",email.or.ci95lo,",",email.or.ci95hi,")"," (",email.or.ci90lo,",",email.or.ci90hi,")","\n",sep="")

    # effect of choice/captive (within high and low strata)
    ## low importance
    lotab <- with(subset(data,rand.imp==0),table(email,captive))
    choice.lo=lotab[2,1]/(lotab[1,1]+lotab[2,1])
    captive.lo=lotab[2,2]/(lotab[1,2]+lotab[2,2])
    or.lo <- round(choice.lo/captive.lo,2)
    log.lo=log(or.lo)
    se.log.lo=sqrt((1/lotab[1,1])+(1/lotab[2,1])+(1/lotab[1,2])+(1/lotab[2,2])) #se for log-odds
    or.lo.cilo=round(exp(log.lo-1.959*se.log.lo),2) #CI in log terms
    or.lo.ciup=round(exp(log.lo+1.959*se.log.lo),2)
    #Odds-Ratio Results
    cat("Odds (Choice): ",round(choice.lo,2),"\n",
        "Odds (Captive): ",round(captive.lo,2),"\n",
        "Low Imp. Odds-Ratio: ",or.lo," (",or.lo.cilo,",",or.lo.ciup,")","\n",sep="")
    
    ## high importance
    hitab <- with(subset(data,rand.imp==1),table(email,captive))
    choice.hi=hitab[2,1]/(hitab[1,1]+hitab[2,1])
    captive.hi=hitab[2,2]/(hitab[1,2]+hitab[2,2])
    or.hi <- round(choice.hi/captive.hi,2)
    log.hi=log(or.hi)
    se.log.hi=sqrt((1/hitab[1,1])+(1/hitab[2,1])+(1/hitab[1,2])+(1/hitab[2,2])) #se for log-odds
    or.hi.cilo=round(exp(log.hi-1.959*se.log.hi),2) #CI in log terms
    or.hi.ciup=round(exp(log.hi+1.959*se.log.hi),2)
    #Odds-Ratio Results
    cat("Odds (Choice): ",round(choice.hi,2),"\n",
        "Odds (Captive): ",round(captive.hi,2),"\n",
        "High Imp. Odds-Ratio: ",or.hi," (",or.hi.cilo,",",or.hi.ciup,")","\n",sep="")

    ## combined
    lohitab <- with(data,table(email,captive))
    choice.lohi=lohitab[2,1]/(lohitab[1,1]+lohitab[2,1])
    captive.lohi=lohitab[2,2]/(lohitab[1,2]+lohitab[2,2])
    or.lohi <- round(choice.lohi/captive.lohi,2)
    log.lohi=log(or.lohi)
    se.log.lohi=sqrt((1/lohitab[1,1])+(1/lohitab[2,1])+(1/lohitab[1,2])+(1/lohitab[2,2])) #se for log-odds
    or.lohi.cilo=round(exp(log.lohi-1.959*se.log.lohi),2) #CI in log terms
    or.lohi.ciup=round(exp(log.lohi+1.959*se.log.lohi),2)
    #Odds-Ratio Results
    cat("Odds (Choice): ",round(choice.lohi,2),"\n",
        "Odds (Captive): ",round(captive.lohi,2),"\n",
        "High Imp. Odds-Ratio: ",or.lohi," (",or.lohi.cilo,",",or.lohi.ciup,")","\n",sep="")
})
