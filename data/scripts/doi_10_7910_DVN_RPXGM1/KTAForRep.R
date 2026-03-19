## KTA Survey Experiment Analysis -- FOR REPLICATION FILE
### Last Update: 2021/08/11

### R Version 3.4.2


#################################################

#choose the current working directory or write your own line of code:

setwd("{insert your working directory path}")


### Packages

install.packages("{insert package names here if needed}") # comment out this line if not needed.

library(Hmisc)
library(interplot)
library(ggplot2)
library(gridExtra)
library(mediation)
library(htmlTable)
library(htmltools)
library(texreg)


### Global Options

options(scipen=999)
options(digits=6)

# error bar function

error.bar <- function(x, y, upper, lower, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}


 


### load data


data <- read.table(choose.files(), sep="\t", header=TRUE) #file is honduras.txt

names(data)




#################### Main Paper Analysis, Tables, and Figures

# Descriptive Statistics reported in 'Data and Methods' section:

# outcome (intention to act)
describe(data$probable) # range, mean, median
sd(data$probable[!is.na(data$probable)]) # standard deviation

# salience
describe(data$relevante)
sd(data$relevante[!is.na(data$relevante)])

# credibility
describe(data$creible)
sd(data$creible[!is.na(data$creible)])

# legitimacy
describe(data$legitima)
sd(data$legitima[!is.na(data$legitima)])


############TABLE 1


########all treats, ols


###intention to act



##treatments

mod.ols.act.alltreats.base <- lm(probable ~ treat_alc_quant + treat_moh_narr + treat_moh_quant, data=data)
summary(mod.ols.act.alltreats.base)

mod.ols.act.alltreats.full <- lm(probable ~ treat_alc_quant + treat_moh_narr + treat_moh_quant + decentralized + female + age + educ + cargo_provider + workadolescents + tandiferente, data=data)
summary(mod.ols.act.alltreats.full)



###salience

mod.ols.sal.alltreats.base <- lm(relevante ~ treat_alc_quant + treat_moh_narr + treat_moh_quant, data=data)
summary(mod.ols.sal.alltreats.base)

mod.ols.sal.alltreats.full <- lm(relevante ~ treat_alc_quant + treat_moh_narr + treat_moh_quant + decentralized + female + age + educ + cargo_provider + workadolescents + tandiferente, data=data)
summary(mod.ols.sal.alltreats.full)



###credibility

mod.ols.cre.alltreats.base <- lm(creible ~ treat_alc_quant + treat_moh_narr + treat_moh_quant, data=data)
summary(mod.ols.cre.alltreats.base)

mod.ols.cre.alltreats.full <- lm(creible ~ treat_alc_quant + treat_moh_narr + treat_moh_quant + decentralized + female + age + educ + cargo_provider + workadolescents + tandiferente, data=data)
summary(mod.ols.cre.alltreats.full)



###legitimacy

mod.ols.leg.alltreats.base <- lm(legitima ~ treat_alc_quant + treat_moh_narr + treat_moh_quant, data=data)
summary(mod.ols.leg.alltreats.base)

mod.ols.leg.alltreats.full <- lm(legitima ~ treat_alc_quant + treat_moh_narr + treat_moh_quant + decentralized + female + age + educ + cargo_provider + workadolescents + tandiferente, data=data)
summary(mod.ols.leg.alltreats.full)



########format, ols



###intention to act

mod.ols.act.format.base <- lm(probable ~ format_quant, data=data)
summary(mod.ols.act.format.base)

mod.ols.act.format.full <- lm(probable ~ format_quant + decentralized + female + age + educ + cargo_provider + workadolescents + tandiferente, data=data)
summary(mod.ols.act.format.full)



###salience

mod.ols.sal.format.base <- lm(relevante ~ format_quant, data=data)
summary(mod.ols.sal.format.base)

mod.ols.sal.format.full <- lm(relevante ~ format_quant + decentralized + female + age + educ + cargo_provider + workadolescents + tandiferente, data=data)
summary(mod.ols.sal.format.full)



###credibility

mod.ols.cre.format.base <- lm(creible ~ format_quant, data=data)
summary(mod.ols.cre.format.base)

mod.ols.cre.format.full <- lm(creible ~ format_quant + decentralized + female + age + educ + cargo_provider + workadolescents + tandiferente, data=data)
summary(mod.ols.cre.format.full)



###legitimacy

mod.ols.leg.format.base <- lm(legitima ~ format_quant, data=data)
summary(mod.ols.leg.format.base)

mod.ols.leg.format.full <- lm(legitima ~ format_quant + decentralized + female + age + educ + cargo_provider + workadolescents + tandiferente, data=data)
summary(mod.ols.leg.format.full)



########source, ols



###intention to act

mod.ols.act.source.base <- lm(probable ~ source_moh, data=data)
summary(mod.ols.act.source.base)

mod.ols.act.source.full <- lm(probable ~ source_moh + decentralized + female + age + educ + cargo_provider + workadolescents + tandiferente, data=data)
summary(mod.ols.act.source.full)



###salience

mod.ols.sal.source.base <- lm(relevante ~ source_moh, data=data)
summary(mod.ols.sal.source.base)

mod.ols.sal.source.full <- lm(relevante ~ source_moh + decentralized + female + age + educ + cargo_provider + workadolescents + tandiferente, data=data)
summary(mod.ols.sal.source.full)



###credibility

mod.ols.cre.source.base <- lm(creible ~ source_moh, data=data)
summary(mod.ols.cre.source.base)

mod.ols.cre.source.full <- lm(creible ~ source_moh + decentralized + female + age + educ + cargo_provider + workadolescents + tandiferente, data=data)
summary(mod.ols.cre.source.full)



###legitimacy

mod.ols.leg.source.base <- lm(legitima ~ source_moh, data=data)
summary(mod.ols.leg.source.base)

mod.ols.leg.source.full <- lm(legitima ~ source_moh + decentralized + female + age + educ + cargo_provider + workadolescents + tandiferente, data=data)
summary(mod.ols.leg.source.full)



#Main Paper TABLE 1

htmlreg(list(mod.ols.sal.source.base,
             mod.ols.sal.format.base,
             mod.ols.sal.alltreats.base,
             mod.ols.cre.source.base,
             mod.ols.cre.format.base,
             mod.ols.cre.alltreats.base,
             mod.ols.leg.source.base,
             mod.ols.leg.format.base,
             mod.ols.leg.alltreats.base,
             mod.ols.act.source.base,
             mod.ols.act.format.base,
             mod.ols.act.alltreats.base),
        file="kta_table1.html",
        stars=c(0.01, 0.05, 0.10),
        caption="SCL&A on Knowledge Provided by Experimental Treatments (Base Models)",
        dcolumn=FALSE,
        custom.model.names=c("S", "S", "S", "C", "C", "C", "L", "L", "L", "A", "A", "A"),
        reorder.coef=c(2,3,4,5,6,1),
        caption.above=TRUE)






############FIGURE 1


#Interplot marginal effects plots to be combined

mod.ols.act.interact.sc.full <- lm(probable ~ relevante + creible + relevante:creible + decentralized + female + age + educ + cargo_provider + workadolescents + tandiferente, data=data)
summary(mod.ols.act.interact.sc.full)

mod.ols.act.interact.sl.full <- lm(probable ~ relevante + legitima + relevante:legitima + decentralized + female + age + educ + cargo_provider + workadolescents + tandiferente, data=data)
summary(mod.ols.act.interact.sl.full)

mod.ols.act.interact.cl.full <- lm(probable ~ creible + legitima + creible:legitima + decentralized + female + age + educ + cargo_provider + workadolescents + tandiferente, data=data)
summary(mod.ols.act.interact.cl.full)


pdf("kta_fig1_top.pdf", width=10, height=6)

##Figure 1, top panel

plot.sc <- interplot(mod.ols.act.interact.sc.full, "relevante", "creible", hist=TRUE) + xlab("Credibility") + ylab("Effect on Intention to Act") + theme_bw() + geom_hline(yintercept=0, linetype="dashed") + ggtitle("Effect of Salience on Action over range of Credibility") + theme(plot.title = element_text(hjust=0.5)) + ylim(-1,1)

plot.sl <- interplot(mod.ols.act.interact.sl.full, "relevante", "legitima", hist=TRUE) + xlab("Legitimacy") + ylab("Effect on Intention to Act") + theme_bw() + geom_hline(yintercept=0, linetype="dashed") + ggtitle("Effect of Salience on Action over range of Legitimacy") + theme(plot.title = element_text(hjust=0.5)) + ylim(-1,1)

grid.arrange(plot.sc, plot.sl, ncol=2)
dev.off()

##Figure 1, middle panel

pdf("kta_fig1_mid.pdf", width=10, height=6)
plot.cs <- interplot(mod.ols.act.interact.sc.full, "creible", "relevante", hist=TRUE) + xlab("Salience") + ylab("Effect on Intention to Act") + theme_bw() + geom_hline(yintercept=0, linetype="dashed") + ggtitle("Effect of Credibility on Action over range of Salience") + theme(plot.title = element_text(hjust=0.5)) + ylim(-1,1)

plot.cl <- interplot(mod.ols.act.interact.cl.full, "creible", "legitima", hist=TRUE) + xlab("Legitimacy") + ylab("Effect on Intention to Act") + theme_bw() + geom_hline(yintercept=0, linetype="dashed") + ggtitle("Effect of Credibility on Action over range of Legitimacy") + theme(plot.title = element_text(hjust=0.5)) + ylim(-1,1)

grid.arrange(plot.cs, plot.cl, ncol=2)
dev.off()


##Figure 1, bottom panel
pdf("kta_fig1_end.pdf", width=10, height=6)
plot.ls <- interplot(mod.ols.act.interact.sl.full, "legitima", "relevante", hist=TRUE) + xlab("Salience") + ylab("Effect on Intention to Act") + theme_bw() + geom_hline(yintercept=0, linetype="dashed") + ggtitle("Effect of Legitimacy on Action over range of Salience") + theme(plot.title = element_text(hjust=0.5)) + ylim(-1.5,1)

plot.lc <- interplot(mod.ols.act.interact.cl.full, "legitima", "creible", hist=TRUE) + xlab("Credibility") + ylab("Effect on Intention to Act") + theme_bw() + geom_hline(yintercept=0, linetype="dashed") + ggtitle("Effect of Legitimacy on Action over range of Credibilty") + theme(plot.title = element_text(hjust=0.5)) + ylim(-1.5,1)

grid.arrange(plot.ls, plot.lc, ncol=2)
dev.off()



############ Main Paper TABLE 2

### Note: the bootstrapping involved in generating the causal mediation estimates means that the estimates on this
### table will vary slightly every time you generate them.

#create interacted mediator for causal mediation

data$medint <- (data$relevante+data$legitima+data$creible+
                  (data$relevante*data$creible)+
                  (data$relevante*data$legitima)+
                  (data$creible*data$legitima)+
                  (data$relevante*data$legitima*data$creible))/10000


# models

# MOH sender
intsource.med <- lm(rel_cre_leg~source_moh, data=data, subset=(!is.na(probable)))
intsource.out <- lm(probable~rel_cre_leg+source_moh, data=data)
intsource.cm <- mediate(intsource.med, intsource.out, treat="source_moh", mediator="rel_cre_leg")
summary(intsource.cm)

# statistical format
intform.med <- lm(rel_cre_leg~format_quant, data=data, subset=(!is.na(probable)))
intform.out <- lm(probable~rel_cre_leg+format_quant, data=data)
intform.cm <- mediate(intform.med, intform.out, treat="format_quant", mediator="rel_cre_leg")
summary(intform.cm)

# specific treatments:
# tech-narr
inttn.med <- lm(rel_cre_leg~treat_moh_narr, data=data, subset=(!is.na(probable)))
inttn.out <- lm(probable~rel_cre_leg+treat_moh_narr, data=data)
inttn.cm <- mediate(inttn.med, inttn.out, treat="treat_moh_narr", mediator="rel_cre_leg")
summary(inttn.cm)

# tech-stat
intts.med <- lm(rel_cre_leg~treat_moh_quant, data=data, subset=(!is.na(probable)))
intts.out <- lm(probable~rel_cre_leg+treat_moh_quant, data=data)
intts.cm <- mediate(intts.med, intts.out, treat="treat_moh_quant", mediator="rel_cre_leg")
summary(intts.cm)

# pol-narr
intpn.med <- lm(rel_cre_leg~treat_alc_narr, data=data, subset=(!is.na(probable)))
intpn.out <- lm(probable~rel_cre_leg+treat_alc_narr, data=data)
intpn.cm <- mediate(intpn.med, intpn.out, treat="treat_alc_narr", mediator="rel_cre_leg")
summary(intpn.cm)

# pol-stat
intps.med <- lm(rel_cre_leg~treat_alc_quant, data=data, subset=(!is.na(probable)))
intps.out <- lm(probable~rel_cre_leg+treat_alc_quant, data=data)
intps.cm <- mediate(intps.med, intps.out, treat="treat_alc_quant", mediator="rel_cre_leg")
summary(intps.cm)

# functions for formatting table output
decim <- function(x,k) trimws(format(round(x, k), nsmall=k))

propmed <- function(x) {
  m <- paste(decim((100*abs(x$d.avg)/(abs(x$d.avg)+abs(x$z.avg))), 1), "%", sep="")
  m}


# generate table 2
table2<- htmlTable(matrix(c(decim(intsource.cm$d.avg, 3),
          paste("p=", intsource.cm$d.avg.p),
          decim(intform.cm$d.avg, 3),
          paste("p=", intform.cm$d.avg.p),
          decim(inttn.cm$d.avg, 3),
          paste("p=", inttn.cm$d.avg.p),
          decim(intts.cm$d.avg, 3),
          paste("p=", intts.cm$d.avg.p),
          decim(intpn.cm$d.avg, 3),
          paste("p=", intpn.cm$d.avg.p),
          decim(intps.cm$d.avg, 3),
          paste("p=", intps.cm$d.avg.p),
          
          decim(intsource.cm$z.avg, 3),
          paste("p=", intsource.cm$z.avg.p),
          decim(intform.cm$z.avg, 3),
          paste("p=", intform.cm$z.avg.p),
          decim(inttn.cm$z.avg, 3),
          paste("p=", inttn.cm$z.avg.p),
          decim(intts.cm$z.avg, 3),
          paste("p=", intts.cm$z.avg.p),
          decim(intpn.cm$z.avg, 3),
          paste("p=", intpn.cm$z.avg.p),
          decim(intps.cm$z.avg, 3),
          paste("p=", intps.cm$z.avg.p),
          
          decim(intsource.cm$tau.coef, 3),
          paste("p=", intsource.cm$tau.p),
          decim(intform.cm$tau.coef, 3),
          paste("p=", intform.cm$tau.p),
          decim(inttn.cm$tau.coef, 3),
          paste("p=", inttn.cm$tau.p),
          decim(intts.cm$tau.coef, 3),
          paste("p=", intts.cm$tau.p),
          decim(intpn.cm$tau.coef, 3),
          paste("p=", intpn.cm$tau.p),
          decim(intps.cm$tau.coef, 3),
          paste("p=", intps.cm$tau.p),
          
          propmed(intsource.cm),
          "",
          propmed(intform.cm),
          "",
          propmed(inttn.cm),
          "",
          propmed(intts.cm),
          "",
          propmed(intpn.cm),
          "",
          propmed(intps.cm),
          "",
          
          intsource.cm$nobs,
          "",
          intform.cm$nobs,
          "",
          inttn.cm$nobs,
          "",
          intts.cm$nobs,
          "",
          intpn.cm$nobs,
          "",
          intps.cm$nobs,
          ""),
          ncol=5,
          dimnames=list(c("Technocratic Sender", "",
                          "Statistical Presentation", "",
                          "Tech-Narr", "",
                          "Tech-Stat", "",
                          "Pol-Narr", "",
                          "Pol-Stat", ""),
                        c("Mediated Effect",
                          "Direct Effect",
                          "Total Effect",
                          "Proportion Mediated",
                          "Num. Obs."))))

# export as html file:
save_html(html=table2, file="kta_table2.html")






#####Prep for Interactions Figures


rel_lo <- quantile(data$relevante, probs=0.05, na.rm=TRUE)
rel_mi <- quantile(data$relevante, probs=0.50, na.rm=TRUE)
rel_hi <- quantile(data$relevante, probs=0.95, na.rm=TRUE)

cre_lo <- quantile(data$creible, probs=0.05, na.rm=TRUE)
cre_mi <- quantile(data$creible, probs=0.50, na.rm=TRUE)
cre_hi <- quantile(data$creible, probs=0.95, na.rm=TRUE)

leg_lo <- quantile(data$legitima, probs=0.05, na.rm=TRUE)
leg_mi <- quantile(data$legitima, probs=0.50, na.rm=TRUE)
leg_hi <- quantile(data$legitima, probs=0.95, na.rm=TRUE)

rel_mean <- mean(na.omit(data$relevante))
cre_mean <- mean(na.omit(data$creible))
leg_mean <- mean(na.omit(data$legitima))





############FIGURE 2



##legitimacy X gender -> action 

mod.ols.act.genleg.base <- lm(probable ~ legitima + female + leg_female, data=data)
summary(mod.ols.act.genleg.base)



## Simulate Coefficients ##
# Seed and number of repetitions
set.seed(19850824)
m <- 100000

# Simulate coefficients from a multivariate normal
betas <- mod.ols.act.genleg.base$coef
vcv <- vcov(mod.ols.act.genleg.base)
sim.betas <- mvrnorm(m, betas, vcv)

# Check to see if the simulated coefficients look like the real results
round(mod.ols.act.genleg.base$coef, digits = 2)
round(head(sim.betas, 10), digits = 2)
data.frame(sim.means = apply(sim.betas, 2, mean), betas = betas, sim.sd = apply(sim.betas, 2, sd), se = sqrt(diag(vcv)))   



## Expected Count Plot ##

# Create hypothetical independent variable profile

male_loleg.data <- data.frame(intercept=1, legitima=quantile(na.omit(data$legitima), 0.15), female=0, leg_female=0)

male_hileg.data <- data.frame(intercept=1, legitima=max(na.omit(data$legitima)), female=0, leg_female=0)

female_loleg.data <- data.frame(intercept=1, legitima=quantile(na.omit(data$legitima), 0.15), female=1, leg_female=min(na.omit(data$legitima)))

female_hileg.data <- data.frame(intercept=1, legitima=max(na.omit(data$legitima)), female=1, leg_female=max(na.omit(data$legitima)))



# Compute the expected values and confidence intervals using the simulated coefficients

ec.sim <- matrix(NA, nrow = m, ncol = 1)

for(i in 1:m){
  ec.sim[i, ] <- as.matrix(male_loleg.data)%*%sim.betas[i, ]
}

pe.maleloleg <- apply(ec.sim, 2, mean)
lo.maleloleg <- apply(ec.sim, 2, quantile, prob = .025)
hi.maleloleg <- apply(ec.sim, 2, quantile, prob = .975)


ec.sim <- matrix(NA, nrow = m, ncol = 1)

for(i in 1:m){
  ec.sim[i, ] <- as.matrix(male_hileg.data)%*%sim.betas[i, ]
}

pe.malehileg <- apply(ec.sim, 2, mean)
lo.malehileg <- apply(ec.sim, 2, quantile, prob = .025)
hi.malehileg <- apply(ec.sim, 2, quantile, prob = .975)


ec.sim <- matrix(NA, nrow = m, ncol = 1)

for(i in 1:m){
  ec.sim[i, ] <- as.matrix(female_loleg.data)%*%sim.betas[i, ]
}

pe.femaleloleg <- apply(ec.sim, 2, mean)
lo.femaleloleg <- apply(ec.sim, 2, quantile, prob = .025)
hi.femaleloleg <- apply(ec.sim, 2, quantile, prob = .975)


ec.sim <- matrix(NA, nrow = m, ncol = 1)

for(i in 1:m){
  ec.sim[i, ] <- as.matrix(female_hileg.data)%*%sim.betas[i, ]
}

pe.femalehileg <- apply(ec.sim, 2, mean)
lo.femalehileg <- apply(ec.sim, 2, quantile, prob = .025)
hi.femalehileg <- apply(ec.sim, 2, quantile, prob = .975)



#barplot of expected values

pe.maleloleg
pe.malehileg
pe.femaleloleg
pe.femalehileg



#plot order

treats.pe <- matrix(c(pe.maleloleg, pe.malehileg, pe.femaleloleg, pe.femalehileg),4,1,byrow=TRUE)

treats.lo <- matrix(c(lo.maleloleg, lo.malehileg, lo.femaleloleg, lo.femalehileg),4,1,byrow=TRUE)
treats.hi <- matrix(c(hi.maleloleg, hi.malehileg, hi.femaleloleg, hi.femalehileg),4,1,byrow=TRUE)

treats.lower <- treats.pe-treats.lo
treats.upper <- treats.hi-treats.pe


pdf("kta_fig2.pdf")
par(mar = c(2.5, 5, 1, .1))

bplot.treats <- barplot(treats.pe, beside=TRUE, space=0.3, ylim=c(0,100), ylab="Expected Intention to Act on Knowledge", names.arg=c("Male \n Low Legitimacy", "Male \n High Legitimacy", "Female \n Low Legitimacy", "Female \n High Legitimacy"), cex.lab=1.3, cex.names=1.1, col=c("gray85","gray85", "gray45", "gray45"), border=c("gray85","gray85", "gray45", "gray45"),  args.legend=list(x="top", bty="n", horiz=TRUE, border=c(c("gray85","gray85", "gray45", "gray45"))))

error.bar(bplot.treats, treats.pe, treats.upper, treats.lower)

dev.off()







######################################################### END
