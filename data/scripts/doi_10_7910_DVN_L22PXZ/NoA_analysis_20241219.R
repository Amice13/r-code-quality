### Attribution to anthropogenic causes helps prevent adverse events
## Diekert, Goeschl, König-Kersting
## code produces statistics in manuscript, in order of appearance
## last edited by florian on December 19, 2024

######-------######-------######
### clear workspace!
rm(list = ls(all = TRUE))
### load necessary packages
library(effsize)
library(stargazer) # for pretty regression tables in SI
library(xtable) # for printing tables
library(MKinfer) # for bootstrapping t.test
library(AER) # for tobit regressions of willingness to pay data
######-------######-------######


######-------######-------######
## read in data
### set wd 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # This command sets the wd to the directory of the script

d <- read.csv(file="NoA_data.csv",header=T,sep=",")

######-------######-------######
## total number of participants
dim(d)[1]


######-------######-------######
## share of people choosing B (high stress) in round 1
sum(d$action_b_r1[d$treat=="BL"])/length(d$action_b_r1[d$treat=="BL"])

## number of participants with relevant experimental histories (biographies in text)
relhist <- c("BAr","BBr","BBg") 
dim(d[d$hist%in%c(c("BAr","BBr","BBg")),])[1]
rm(relhist)

## number of particpants in FI treatment
dim(d[d$treat=="FI",])[1]


######-------######-------############-------######-------######

######-------######-------######
## making barplots showing share of switchers
relhist2 <- c("BAr","BBr","BBg") #
causenames <- c("No.pool","Natural","Anthrop","Joint","Natural.no","Anthrop.no","Joint.no")

numb_switcher <- rep(NA,7)
names(numb_switcher) <- causenames
numb_total <- numb_switcher 

#### analyse switchers including IM treatment
## adverse event but no attribution
numb_switcher[1] <- sum(d$switcher[d$hist%in%relhist2&(d$treat=="IM"&d$info_bought==0|d$treat=="FI")])
numb_total[1] <- length(d$switcher[d$hist%in%relhist2&(d$treat=="IM"&d$info_bought==0|d$treat=="FI")])
## attribution: natural cause
numb_switcher[2] <- sum(d$switcher[d$hist=="BAr"&(d$treat=="IM"&d$info_bought==1|d$treat=="BL")])
numb_total[2] <- length(d$switcher[d$hist=="BAr"&(d$treat=="IM"&d$info_bought==1|d$treat=="BL")])
## attribution: human cause
numb_switcher[3] <- sum(d$switcher[d$hist=="BBg"&(d$treat=="IM"&d$info_bought==1|d$treat=="BL")])
numb_total[3] <- length(d$switcher[d$hist=="BBg"&(d$treat=="IM"&d$info_bought==1|d$treat=="BL")])
## attribution: joint cause
numb_switcher[4] <- sum(d$switcher[d$hist=="BBr"&(d$treat=="IM"&d$info_bought==1|d$treat=="BL")])
numb_total[4] <- length(d$switcher[d$hist=="BBr"&(d$treat=="IM"&d$info_bought==1|d$treat=="BL")])

#### additional analysis (not in paper, difference by cause if not attributable)
## no attribution: natural cause
numb_switcher[5] <- sum(d$switcher[d$hist=="BAr"&(d$treat=="IM"&d$info_bought==0|d$treat=="FI")])
numb_total[5] <- length(d$switcher[d$hist=="BAr"&(d$treat=="IM"&d$info_bought==0|d$treat=="FI")])
## no attribution: human cause
numb_switcher[6] <- sum(d$switcher[d$hist=="BBg"&(d$treat=="IM"&d$info_bought==0|d$treat=="FI")])
numb_total[6] <- length(d$switcher[d$hist=="BBg"&(d$treat=="IM"&d$info_bought==0|d$treat=="FI")])
## no attribution: joint cause
numb_switcher[7] <- sum(d$switcher[d$hist=="BBr"&(d$treat=="IM"&d$info_bought==0|d$treat=="FI")])
numb_total[7] <- length(d$switcher[d$hist=="BBr"&(d$treat=="IM"&d$info_bought==0|d$treat=="FI")])

######-------######-------######

######-------######-------######
share_switchers <- numb_switcher/numb_total
share_switchers.SE <- sqrt(share_switchers*(1-share_switchers)/numb_total)
######-------######-------######


######-------######-------######
## defining colors for barplots
Natcol.bg <- rgb(80,151,171, maxColorValue=255, alpha=255) # petrol
Humcol.bg <- rgb(255,165,0, maxColorValue=255, alpha=255) # international orange
Compcol.bg <- gray(0.5)
######-------######-------######

######-------######-------######
## make barplot figure
pdf("output/barplot_NoA_reaction_to_cause.pdf",width=6,height=4.5) #width in inches

par(mfrow=c(1,1),
    mar = c(2.5, 4, 1.5, 2)) # make the plots be closer together
# mar:  bottom, left, top, and right

########## for this plot, drop the info from the not-attributable cause
share_switchers <- share_switchers[1:4]
share_switchers.SE <- share_switchers.SE[1:4]
########

barCenters <- barplot(height = share_switchers,
                      names.arg=c("No attribution","Natural cause","Anthrop. cause","Joint cause"),
                      cex.names=0.8,
                      beside = true, las = 1,
                      ylim = c(0, 0.6),
                      #main="Study 3",
                      ylab = "Share of switchers",
                      border = "black",
                      col = c(Compcol.bg,Natcol.bg,Humcol.bg,Compcol.bg),
                      density=c(10,1000,1000,1000))

#text(x=barCenters[2,1]-barCenters[1,1], y= 0.57, labels="BL&IM_see vs FI&IM_nosee", adj=0.3, font =2, cex=1.2)

arrows(barCenters, share_switchers+share_switchers.SE, barCenters,
       share_switchers-share_switchers.SE, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

segments(x0=1.3,y0=-10,x1=1.3,y1=10,lwd=3,lty="dashed")

#### closing this figure
dev.off()
######-------######-------######

### clear workspace!
rm(list=setdiff(ls(), c("d","numb_switcher","numb_total")))


######-------######-------######
## shares of participants reducing stress reported in text

## share of players that switch from H to L after adverse event in absence of attributabilty
share_switchers <- numb_switcher/numb_total
share_switchers[1]


## Comparing the propensity to reduce stress without causal attribution and after attribution to an anthropogenic cause
## for the same history 
prop.test(x = numb_switcher[c(6,3)], n = numb_total[c(6,3)])
## all histories pooled
prop.test(x = numb_switcher[c(1,3)], n = numb_total[c(1,3)])


## Comparing the propensity to reduce anthropogenic stress on the system without causal attribution and after attribution to a natural cause
## for the same history 
prop.test(x = numb_switcher[c(5,2)], n = numb_total[c(5,2)])
## all histories pooled
prop.test(x = numb_switcher[c(1,2)], n = numb_total[c(1,2)])



## Comparing the propensity to reduce anthropogenic stress on the system without causal attribution and after attribution to a joint cause
## for the same history 
prop.test(x = numb_switcher[c(7,4)], n = numb_total[c(7,4)])
## all histories pooled
prop.test(x = numb_switcher[c(1,4)], n = numb_total[c(1,4)])

######-------######-------############-------######-------######


######-------######-------######
## Regression analysis

######-------######-------######
## make the relevant sample
relhist2 <- c("BAr","BBr","BBg") ## question: which are the relevant histories?

drel <- d[d$hist%in%relhist2,]
drel$knows <- ifelse(drel$treat=="IM"&drel$info_bought==0|drel$treat=="FI",0,1)

drel$nat.cause <- ifelse(drel$hist=="BAr"&drel$knows==1,1,0)
drel$anthro.cause <- ifelse(drel$hist=="BBg"&drel$knows==1,1,0)
drel$joint.cause <- ifelse(drel$hist=="BBr"&drel$knows==1,1,0)

mod1.base <- lm(switcher ~   nat.cause+anthro.cause+joint.cause, data=drel)
mod2.base <- glm(switcher ~   nat.cause+anthro.cause+joint.cause, data=drel,family = binomial(link = "probit"))

mod1.demographic <- lm(switcher ~   nat.cause+anthro.cause+joint.cause+ age + female + risk_preference + education, data=drel)
mod2.demographic <- glm(switcher ~   nat.cause+anthro.cause+joint.cause+ age + female + risk_preference + education, data=drel,family = binomial(link = "probit"))

mod1.demographic.addcontrol <- lm(switcher ~   nat.cause+anthro.cause+joint.cause+ age + female + risk_preference + education+ comprehension_attempts + time_instructions, data=drel)
mod2.demographic.addcontrol <- glm(switcher ~   nat.cause+anthro.cause+joint.cause+ age + female + risk_preference + education+ comprehension_attempts + time_instructions, data=drel,family = binomial(link = "probit"))

######-------######-------######
## produce tex table output
stargazer(mod2.base,mod2.demographic,mod2.demographic.addcontrol,
          covariate.labels = 
            c("Nat.cause", "Anthro.cause", "Joint cause", "Age",
              "Female", "Risk tolerance", "Education", "Comprehension", "Speed", "Constant"),
          omit.stat = c("f","rsq","ser"),
          title="Probit regression to explain reduction of anthropogenic stress",label="switching-probit",
          notes.append = FALSE,
          notes="Text to be replaced manually",
          out="output/table2.tex")
######-------######-------######


######-------######-------######

### clear workspace!
rm(list=setdiff(ls(), c("d","numb_switcher","numb_total","relhist2")))
######-------######-------######


######-------######-------############-------######-------######
## Result 2: Causal attribution effect
prop.test(x = numb_switcher[c(3,2)], n = numb_total[c(3,2)])

######-------######-------######


######-------######-------############-------######-------######
## Result 3 : Demand for causal attribution

# H3: Non-informative causal feedback after an adverse event attracts an average bid of zero.
y1 <- d$wtp[d$treatment=="IM"&d$wants_to_know==1&d$hist%in%c("BBg","BAr","BBr")]
y1.1 <- y1[y1>0]
## average bid in FI treatment, diff from zero? 
t.test(y1)
# boot-strapped t-test to account for testing boundary value
boot.t.test(y1, mu = 0, alternative = "greater", conf.level = 0.95, R = 10000)

y2 <- d$wtp[d$treatment=="FI"&d$wants_to_know==1&d$hist%in%c("BBg","BAr","BBr")]
y2.1 <- y2[y2>0]

## average bid in FI treatment, diff from zero?
t.test(y2)
boot.t.test(y2, mu = 0, alternative = "greater", conf.level = 0.95, R = 10000)

### alternative procedure to test for beta>0 when b in [0,1]
# first do ols on constant
mod.b <- lm(y1~1)
res <- summary(mod.b)
# then do a one-sided test whether the coefficient is larger than zero
pt(coef(res)[, 3], mod.b$df, lower = FALSE)

mod.b2 <- lm(y2~1)
res2 <- summary(mod.b2)
# then do a one-sided test whether the coefficient is larger than zero
pt(coef(res2)[, 3], mod.b2$df, lower = FALSE)


## difference between demand after round 1 and round 2?
t.test(y1,y2)
boot.t.test(y1, y2, alternative = "two.sided", conf.level = 0.95, R = 10000)

## difference between demand after round 1 and round 2, conditional on wanting to pay?
t.test(y1.1,y2.1)
boot.t.test(y1.1, y2.1, alternative = "two.sided", conf.level = 0.95, R = 10000)

## difference at the extensive margin
x1 <- sum(d$wants_to_know[d$treat=="IM"])
n1 <- length(d$wants_to_know[d$treat=="IM"])          
x2 <- sum(d$wants_to_know[d$treat=="FI"])
n2 <- length(d$wants_to_know[d$treat=="FI"]   )

## in total: share of those wanting to know:
prop.test(x = x1+x2, n = n1+n2)

## difference in share of those demanding info after initial round (IM) or after final round (FI)
prop.test(x = c(x1, x2), n = c(n1, n2))

## share pf those with a strictly positive demand given that they want to know
prop.test(x = c(length(y1.1), length(y2.1)), n = c(length(y1), length(y2)))

######-------######-------######

######-------######-------######
## Regression analysis of demand for attribution

### clear workspace!
rm(list=setdiff(ls(), c("d")))

#### take a look at data (conditional on wanting to know)
wtp.IM <- d$wtp[d$treatment=="IM"&d$wants_to_know==1]
hist(wtp.IM,breaks=20)
#### we see bunching at zero and maybe at upper end: use tobit regression


#### rename variables and make data subset so that the stargazer command produces tables that are easy to format
## ATtention! this involves naming action B in Round 2 in FI the same as action B in round 1 in IM
## Use the dataframes d.IM and d.IF only in this section of the code!

d$risk_tolerance <- d$risk_preference
d.IM <- d[d$treat=="IM",]
d.FI <- d[d$treat=="FI",]

d.IM$action_b <- d.IM$action_b_r1
d.FI$action_b <- d.FI$action_b_r2

d.FI$adverse <- NULL
d.FI$adverse <- d.FI$adverse.r2

formula1 <- wants_to_know ~  action_b*adverse +  age + female + risk_tolerance + education+ comprehension_attempts+ time_instructions
formula2 <- wtp ~  action_b*adverse + age + female + risk_tolerance + education+ comprehension_attempts+  time_instructions


## regression analysis of wtp
mod.wtp.prob.IM <- lm(formula1 , data=d.IM)
#summary(mod.wtp.prob)

## OLS, for comparison
mod.wtp.IM <- lm(formula2, data=d.IM[d.IM$wants_to_know==1,])
#summary(mod.wtp.IM)
## Tobit
mod.wtp.IM.tobit <-tobit(formula2, data=d.IM[d.IM$wants_to_know==1,], left=0, right=0.5)


mod.wtp.prob.FI <- lm(formula1, data=d.FI)
#summary(mod.wtp.prob.FI)

## OLS, for comparison
mod.wtp.FI <- lm(formula2, data=d.FI[d.FI$wants_to_know==1,])
#summary(mod.wtp.FI)
## Tobit
mod.wtp.FI.tobit <-tobit(formula2, data=d.FI[d.FI$wants_to_know==1,], left=0, right=0.5)



stargazer(mod.wtp.prob.IM, mod.wtp.IM.tobit, mod.wtp.prob.FI, mod.wtp.FI.tobit, 
          covariate.labels = 
            c("Action H", "Adverse", "Action H $\\times$ Adverse", "Age",
              "Female", "Risk tolerance", "Education", "Comprehension", "Speed", "Constant"),
          omit.stat = c("f","rsq","ser","ll","aic"),
          column.labels = c("IM","IM", "FI","FI"), #type="text",
          title="Regressions to explain demand for attribution", label="wtpregression",
          notes.append = FALSE,
          notes="Text to be replaced manually",
          out="output/table3.tex")
######-------######-------######



######-------######-------######
### clear workspace!
rm(list=setdiff(ls(), "d"))
######-------######-------######


######-------######-------############-------######-------######
## creates table on participant characteristics by treatment 

varvect <- c("age","female","education","risk_preference", "comprehension_attempts", "time_instructions")
treatvect <- c("BL","IM","FI")

#### overview table for appendix
tab <- matrix(NA,length(varvect)+1,5)

## mean values
for(j in 1:length(treatvect)){
  for(i in 1:length(varvect)){
    ## define x vectors with special treatment for variables that are transformed to dummies
    x1 <- d[d$treat==treatvect[j],which(names(d)==varvect[i])]
    ## insert mean values in table
    tab[i,j+1] <- mean(x1,na.rm=T)
  }
}

## t-tests
for(i in 1:length(varvect)){
  t1 <- t.test(d[d$treat=="BL",which(names(d)==varvect[i])],
               d[d$treat=="IM",which(names(d)==varvect[i])])
  #tab[i,6] <- t1$p.value
  t2 <- t.test(d[d$treat=="BL",which(names(d)==varvect[i])],
               d[d$treat=="FI",which(names(d)==varvect[i])])
  #tab[i,7] <- t2$p.value
  t3 <- t.test(d[d$treat=="IM",which(names(d)==varvect[i])],
               d[d$treat=="FI",which(names(d)==varvect[i])])
  #   tab[i,8] <- t2$p.value
  p.vals <- c(t1$p.value,
              t2$p.value,
              t3$p.value)
  tab[i,5] <- min(p.adjust(p.vals, method = "hochberg"))
}

## round values
tab[1:6,2:5] <- round(tab[1:6,2:5],2)
## add sample size
tab[7,2:4] <- c(length(d$age[d$treatment=="BL"]),
                length(d$age[d$treatment=="IM"]),
                length(d$age[d$treatment=="FI"]))

## give names to rows and columns
tab[,1] <- c("Age","Female","Education","Risk tolerance", "Comprehension", "Speed", "N")
tab <- as.data.frame(tab)
names(tab) <- c("variable","BL","IM","FI", "min p-val")#,
# "p-val diff HC-CC","p-val diff HC-AA","p-val diff HC-AA/CC",
# "p-val diff CC-AA","p-val diff CC-AA/CC","p-val diff AA-AA/CC")
tabmat <- xtable(tab)
print(tabmat,include.rownames = F,file="output/tableA1.tex")

######-------######-------######
### clear workspace!
rm(list=setdiff(ls(), "d"))
######-------######-------######


######-------######-------############-------######-------######
## creates table on participant characteristics by history and treatment

varvect <- c("age","female","education","risk_preference", "comprehension_attempts", "time_instructions")
treatvect <- c("BL","IM","FI")

######### the following code loops through the different relevant histories
for(i in 1:4){

if(i==1){relhist3 <- c("AAr","ABr")}# player exerts low stress and adverse event:
if(i==2){relhist3 <- c("AAg","ABg")}# player exerts low stress and no adverse event:
if(i==3){relhist3 <- c("BAr","BBg","BBr")}# player exerts high stress and adverse event:
if(i==4){relhist3 <- "BAg"}# player exerts high stress and no adverse event:

  
#### overview table for appendix
tab <- matrix(NA,length(varvect)+1,5)

## mean values
for(j in 1:length(treatvect)){
  for(i in 1:length(varvect)){
    ## define x vectors with special treatment for variables that are transformed to dummies
    x1 <- d[d$treat==treatvect[j]&d$hist%in%relhist3,which(names(d)==varvect[i])]
    ## insert mean values in table
    tab[i,j+1] <- mean(x1,na.rm=T)
  }
}

## t-tests
for(i in 1:length(varvect)){
  t1 <- t.test(d[d$treat=="BL"&d$hist%in%relhist3,which(names(d)==varvect[i])],
               d[d$treat=="IM"&d$hist%in%relhist3,which(names(d)==varvect[i])])
  #tab[i,6] <- t1$p.value
  t2 <- t.test(d[d$treat=="BL"&d$hist%in%relhist3,which(names(d)==varvect[i])],
               d[d$treat=="FI"&d$hist%in%relhist3,which(names(d)==varvect[i])])
  #tab[i,7] <- t2$p.value
  t3 <- t.test(d[d$treat=="IM"&d$hist%in%relhist3,which(names(d)==varvect[i])],
               d[d$treat=="FI"&d$hist%in%relhist3,which(names(d)==varvect[i])])
  #   tab[i,8] <- t2$p.value
  p.vals <- c(t1$p.value,
              t2$p.value,
              t3$p.value)
  tab[i,5] <- min(p.adjust(p.vals, method = "hochberg"))
}

## round values
tab[1:6,2:5] <- round(tab[1:6,2:5],2)
## add sample size
tab[7,2:4] <- c(length(d$age[d$treatment=="BL"&d$hist%in%relhist3]),
                length(d$age[d$treatment=="IM"&d$hist%in%relhist3]),
                length(d$age[d$treatment=="FI"&d$hist%in%relhist3]))

## give names to rows and columns
tab[,1] <- c("Age","Female","Education","Risk tolerance", "Comprehension", "Speed", "N")
tab <- as.data.frame(tab)
names(tab) <- c("variable","BL","IM","FI", "min p-val")#,
# "p-val diff HC-CC","p-val diff HC-AA","p-val diff HC-AA/CC",
# "p-val diff CC-AA","p-val diff CC-AA/CC","p-val diff AA-AA/CC")
if("ABr"%in%relhist3){
  tabmat <- xtable(tab,caption="Participant characteristics by treatment, for players exerting low stress and experiencing the adverse event")
  print(tabmat,include.rownames = F, file="output/tableA2.tex")
}
if("AAg"%in%relhist3){
  tabmat <- xtable(tab,caption="Participant characteristics by treatment, for players exerting low stress and not experiencing the adverse event")
  print(tabmat,include.rownames = F, file="output/tableA3.tex")
}
if("BAr"%in%relhist3){
  tabmat <- xtable(tab,caption="Participant characteristics by treatment, for players exerting high stress and experiencing the adverse event")
  print(tabmat,include.rownames = F, file="output/tableA4.tex")
}
if("BAg"%in%relhist3){
  tabmat <- xtable(tab,caption="Participant characteristics by treatment, for players exerting high stress and not experiencing the adverse event")
  print(tabmat,include.rownames = F, file="output/tableA5.tex")
}

}

######-------######-------######
### clear workspace!
rm(list=setdiff(ls(), "d"))
######-------######-------######


######-------######-------############-------######-------######
## end code
######-------######-------############-------######-------######