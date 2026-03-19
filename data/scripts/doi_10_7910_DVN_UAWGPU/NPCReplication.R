################################################################################
#### Replication code for for "Nonparametric Combination (NPC)", ###############
#### by Devin Caughey, Allan Dafoe, and Jason Seawright ########################
################################################################################

require(foreign)
require(rms)
require(coin)
require(car)
require(xtable)
require(ggplot2)
require(mvtnorm)
require(NPC)
require(plyr)

################################################################################
##### FIGURE 1 (REJECTION REGIONS) #############################################
################################################################################

fisher.reject <- function (p1) {
    ifelse(p1 < 0.0087, 1, 0.0087/p1)
}
normal.reject <- function (p1) 1 - pnorm(2.326 - qnorm(1 - p1))
p <- seq(.01, 0.99, .0001)
p.df <- data.frame(p=p, Product=fisher.reject(p), Normal=normal.reject(p))

pdf("PDF/Rejection.pdf", width=6, height=6)
(ggplot(data.frame(x=c(0, 1)), aes(x=x))
 + stat_function(fun = fisher.reject, linetype = "dotted", n=100)
 + stat_function(fun = normal.reject, linetype = "solid", n=100)
 + annotate("segment", x=1 - sqrt(.95), xend = 1,
            y=1 - sqrt(.95), yend=1 - sqrt(.95), linetype="dashed")
 + annotate("segment", y=1 - sqrt(.95), yend = 1,
            x=1 - sqrt(.95), xend=1 - sqrt(.95), linetype="dashed")
 + annotate("text", x = .125, y = .125, label = "Normal", adj=0, size=4.5)
 + annotate("text", x = .096, y = .096, label = "Product", adj=0, size=4.5)
 + annotate("text", x = .03, y = .034, label = "Minimum", adj=0, size=4.5)
 + coord_cartesian(xlim = c(0, 0.31), ylim = c(0, 0.31))
 + scale_x_continuous(breaks=seq(0, .3, 0.05))
 + scale_y_continuous(breaks=seq(0, .3, 0.05))
 + labs(x="p-value 1", y="p-value 2")
 + theme_bw()
 )
dev.off()


################################################################################
##### ILLUSTRATIVE EXAMPLE #####################################################
################################################################################

## COVARIANCE = CORRELATION = -0.25
cov <- -0.25
N <- 8
Tr <- c(rep(0, 4), rep(1, 4))
d1 <- 1
d2 <- 1
d3 <- 1
sd <- 1
sigma <-
    matrix(c(1, cov, cov, cov, 1, cov, cov, cov, 1), ncol = 3)
## Create error matrix
set.seed(2)
ee <- rmvnorm(N, c(0, 0, 0), sigma)
mean(c(cor(ee)[1, 2], cor(ee)[3, 2], cor(ee)[1, 3]))

Y1 <- -.5 + Tr*d1 + ee[, 1]
Y2 <- -.5 + Tr*d2 + ee[, 2]
Y3 <- -.5 + Tr*d3 + ee[, 3] 
## Create data
(ex.dta <- data.frame(Tr, Y1, Y2, Y3))
(diffs <- round(colMeans(subset(ex.dta, Tr == 1, -Tr)) -
                colMeans(subset(ex.dta, Tr == 0, -Tr)), 2))
mean(diffs)
## NPC
p1normal <- NPC(data=ex.dta, tr.var="Tr", tr.label=1,
                y.vars=c("Y1", "Y2", "Y3"), n.perms=1000,
                alternative = "greater", seed=1, comb.fun="NormalCF",
                test.statistic="DiffMeans", FWE.adj=FALSE)
round(p1normal$p.value, 2) ## one-sided
x1normal <- rbind(ex.dta, c(1, diffs), c(1, p1normal$p.value[-4]))
p1product <- NPC(data=ex.dta, tr.var="Tr", tr.label=1,
          y.vars=c("Y1", "Y2", "Y3"), n.perms=1000,
          alternative = "greater", seed=1, comb.fun="ProductCF",
          test.statistic="DiffMeans", FWE.adj=FALSE)
round(p1product$p.value, 2) ## one-sided
x1product <- rbind(ex.dta, c(1, diffs), c(1, p1product$p.value[-4]))
p1minimum <- NPC(data=ex.dta, tr.var="Tr", tr.label=1,
          y.vars=c("Y1", "Y2", "Y3"), n.perms=1000,
          alternative = "greater", seed=1, comb.fun="MinimumCF",
          test.statistic="DiffMeans", FWE.adj=FALSE)
round(p1minimum$p.value, 2) ## one-sided
x1minimum <- rbind(ex.dta, c(1, diffs), c(1, p1minimum$p.value[-4]))

## T-tests and MANOVA
t.test(Y1 ~ Tr, var.equal = TRUE, alternative = "less")
t.test(Y2 ~ Tr, var.equal = TRUE, alternative = "less")
t.test(Y3 ~ Tr, var.equal = TRUE, alternative = "less")
Anova(lm(cbind(Y1, Y2, Y3) ~ Tr)) ## two-sided

## COVARIANCE = CORRELATION = 0.25
cov <- 0.25
N <- 8
Tr <- c(rep(0, 4), rep(1, 4))
d1 <- 1
d2 <- 1
d3 <- 1
sd <- 1
sigma <- matrix(c(1, cov, cov, cov, 1, cov, cov, cov, 1), ncol = 3)
## Create error matrix
set.seed(2)
ee <- rmvnorm(N, c(0, 0, 0), sigma)
mean(c(cor(ee)[1, 2], cor(ee)[3, 2], cor(ee)[1, 3]))

Y1 <- -.5 + Tr*d1 + ee[, 1]
Y2 <- -.5 + Tr*d2 + ee[, 2]
Y3 <- -.5 + Tr*d3 + ee[, 3] 
## Create data
(ex.dta <- data.frame(Tr, Y1, Y2, Y3))
(diffs <- round(colMeans(subset(ex.dta, Tr == 1, -Tr)) -
                colMeans(subset(ex.dta, Tr == 0, -Tr)), 2))
mean(diffs) 
## NPC
p2normal <- NPC(data=ex.dta, tr.var="Tr", tr.label=1,
          y.vars=c("Y1", "Y2", "Y3"), n.perms=1000,
          alternative = "greater", seed=1, comb.fun="NormalCF",
          test.statistic="DiffMeans", FWE.adj=FALSE)
round(p2normal$p.value, 2) ## one-sided
x2normal <- rbind(ex.dta, c(1, diffs), c(1, p2normal$p.value[-4]))
p2product <- NPC(data=ex.dta, tr.var="Tr", tr.label=1,
          y.vars=c("Y1", "Y2", "Y3"), n.perms=1000,
          alternative = "greater", seed=1, comb.fun="ProductCF",
          test.statistic="DiffMeans", FWE.adj=FALSE)
round(p2product$p.value, 2) ## one-sided
x2product <- rbind(ex.dta, c(1, diffs), c(1, p2product$p.value[-4]))
p2minimum <- NPC(data=ex.dta, tr.var="Tr", tr.label=1,
          y.vars=c("Y1", "Y2", "Y3"), n.perms=1000,
          alternative = "greater", seed=1, comb.fun="MinimumCF",
          test.statistic="DiffMeans", FWE.adj=FALSE)
round(p2minimum$p.value, 2) ## one-sided
x2minimum <- rbind(ex.dta, c(1, diffs), c(1, p2minimum$p.value[-4]))
## T-tests and MANOVA
t.test(Y1 ~ Tr, var.equal = TRUE, alternative = "less")
t.test(Y2 ~ Tr, var.equal = TRUE, alternative = "less")
t.test(Y3 ~ Tr, var.equal = TRUE, alternative = "less")
Anova(lm(cbind(Y1, Y2, Y3) ~ Tr)) ## two-sided

## COVARIANCE = CORRELATION = 1
cov <- 1
N <- 8
Tr <- c(rep(0, 4), rep(1, 4))
d1 <- 1
d2 <- 1
d3 <- 1
sd <- 1
sigma <-
    matrix(c(1, cov, cov, cov, 1, cov, cov, cov, 1), ncol = 3)
## Create error matrix
set.seed(2)
ee <- rmvnorm(N, c(0, 0, 0), sigma)
mean(c(cor(ee)[1, 2], cor(ee)[3, 2], cor(ee)[1, 3]))

Y1 <- -.5 + Tr*d1 + ee[, 1]
Y2 <- -.5 + Tr*d2 + ee[, 2]
Y3 <- -.5 + Tr*d3 + ee[, 3] 
## Create data
(ex.dta <- data.frame(Tr, Y1, Y2, Y3))
(diffs <- round(colMeans(subset(ex.dta, Tr == 1, -Tr)) -
                colMeans(subset(ex.dta, Tr == 0, -Tr)), 2))
mean(diffs)
## NPC
p3normal <- NPC(data=ex.dta, tr.var="Tr", tr.label=1,
          y.vars=c("Y1", "Y2", "Y3"), n.perms=1000,
          alternative = "greater", seed=1, comb.fun="NormalCF",
          test.statistic="DiffMeans", FWE.adj=FALSE)
round(p3normal$p.value, 2) ## one-sided
x3normal <- rbind(ex.dta, c(1, diffs), c(1, p3normal$p.value[-4]))
p3product <- NPC(data=ex.dta, tr.var="Tr", tr.label=1,
          y.vars=c("Y1", "Y2", "Y3"), n.perms=1000,
          alternative = "greater", seed=1, comb.fun="ProductCF",
          test.statistic="DiffMeans", FWE.adj=FALSE)
round(p3product$p.value, 2) ## one-sided
x3product <- rbind(ex.dta, c(1, diffs), c(1, p3product$p.value[-4]))
p3minimum <- NPC(data=ex.dta, tr.var="Tr", tr.label=1,
          y.vars=c("Y1", "Y2", "Y3"), n.perms=1000,
          alternative = "greater", seed=1, comb.fun="MinimumCF",
          test.statistic="DiffMeans", FWE.adj=FALSE)
round(p3minimum$p.value, 2) ## one-sided
x3minimum <- rbind(ex.dta, c(1, diffs), c(1, p3minimum$p.value[-4]))
## T-tests and MANOVA
t.test(Y1 ~ Tr, var.equal = TRUE, alternative = "less")
t.test(Y2 ~ Tr, var.equal = TRUE, alternative = "less")
t.test(Y3 ~ Tr, var.equal = TRUE, alternative = "less")

xtable(cbind(x1product[, -1], x2product[, -1], x3product[, -1]), round=2)
print(xtable(cbind(t(p1product$p.value[-4]), t(p2product$p.value[-4]),
                   t(p3product$p.value[-4]))), include.rownames = FALSE)

################################################################################
#### WANTCHEKON (2003) #########################################################
################################################################################

require(sandwich)
require(lmtest)
require(contrast)

benin.dta <- read.csv("Data/Benin.csv")
benin.dta$Treatment <- factor(benin.dta$Treatment)

round(cor(subset(benin.dta,, c(vote_pop, vote_male, vote_female))), 2)

(ggplot(benin.dta, aes(y=vote_pop, x=reorder(Treatment, vote_pop)))
 + geom_boxplot()
 + stat_summary(fun.y=mean, geom="point", shape=1, size=rel(3))
 + labs(x="Campaign Type", y="Vote Share")
 )

## H1
NPC(data=subset(benin.dta, Treatment != "client"),
    tr.var="Treatment", block.var="District",
    tr.label="both", y.vars="vote_pop",
    n.perms=9999, alternative = "greater", seed=1,
    comb.fun="ProductCF", na.rm=FALSE, FWE.adj=FALSE,
    test.statistic="DiffMeans")

## H2
NPC(data=subset(benin.dta, Treatment != "program"),
    tr.var="Treatment", block.var="District",
    tr.label="client", y.vars="vote_pop",
    n.perms=9999, alternative = "greater", seed=1,
    comb.fun="ProductCF", na.rm=FALSE, FWE.adj=FALSE,
    test.statistic="DiffMeans")

## Dose-Response
want.npc.p <- NPC(data=benin.dta, tr.var="Treatment", block.var="District",
              tr.label=c("program", "client"),
              y.vars=c("vote_pop", "vote_pop"),
              n.perms=9999, alternative = c("less", "greater"), seed=1,
              comb.fun="ProductCF", na.rm=FALSE, FWE.adj=FALSE,
                  test.statistic="DiffMeans")
want.npc.p$p.value

(ols(vote_pop ~ Treatment, benin.dta, x=TRUE))
(ols(vote_pop ~ Treatment + District, benin.dta, x=TRUE))
(ols(vote_pop ~ Treatment, subset(benin.dta, Treatment != "both"), x=TRUE))
(ols(vote_pop ~ Treatment + District,
     subset(benin.dta, Treatment != "both"), x=TRUE))
## 
robcov(ols(vote_pop ~ Treatment, benin.dta, x=TRUE))
robcov(ols(vote_pop ~ Treatment + District, benin.dta, x=TRUE))
## jackknife
robcov(ols(vote_pop ~ Treatment, benin.dta, x=TRUE), method='efron') ## HC3
robcov(ols(vote_pop ~ Treatment + District, benin.dta, x=TRUE), method='efron')

expdir <- c(1, -1)
benin.lm.pop <- lm(vote_pop ~ Treatment ## + District
                 , benin.dta)
(ct.hom <- coeftest(benin.lm.pop))
(wt.hom <- waldtest(benin.lm.pop, 1, test="F"))
p.adjust(sort(ct.hom[2:3, 'Pr(>|t|)']/2), method='holm')

## welch
(ct.het <- coeftest(benin.lm.pop, vcov=vcovHC(benin.lm.pop, type="HC2")))
(wt.het <- waldtest(benin.lm.pop, 1, test="F",
                    vcov=vcovHC(benin.lm.pop, type="HC2")))

ct.hom[, "Std. Error"] - ct.het[, "Std. Error"]
mean(ct.hom[, "Std. Error"]) - mean(ct.het[, "Std. Error"])
## homoskedastic SEs a little larger, so use them

xtable(matrix(c(ct.hom[c("Treatmentprogram", "Treatmentclient"), "Pr(>|t|)"]/2,
                wt.hom[2, "Pr(>F)"])))


################################################################################
##### DAFOE & CAUGHEY (2011) ###################################################
################################################################################

## Load Dafoe & Caughey matched data

honor.dta <- read.dta("Data/honor.dta")
summary(honor.dta)
cor(subset(honor.dta, Southern==0, c("UseOfForce", "Duration", "Outcome")),
    method="kendall")
cor(subset(honor.dta, Southern==1, c("UseOfForce", "Duration", "Outcome")),
    method="kendall")
with(honor.dta,
     robcov(ols(UseOfForce ~ Southern, x=TRUE), cluster=President))
with(honor.dta,
     robcov(ols(Duration ~ Southern, x=TRUE), cluster=President))
with(honor.dta,
     robcov(ols(Outcome ~ Southern, x=TRUE), cluster=President))

NPC(data=honor.dta, tr.var="Southern", tr.label=1,
    y.vars=c("UseOfForce", "Duration", "Outcome"),
    block.var="Pair", clust.var="President", event.var="Event",
    n.perms=9999, alternative = "greater", seed=1,
    comb.fun="NormalCF", test.statistic=c("DiffMeans"),
    na.rm=TRUE, FWE.adj=TRUE, step.down=FALSE)

KSless <- function (y, tr, tl, ...) {
  suppressWarnings(ks.test(y[tr == tl], y[tr != tl], exact = FALSE, 
                           alternative = "less")$statistic)
}
logrank <- function (y, tr, tl, event, block, ...) {
  ## Log-rank statistic
  if (is.null(block)) {
    lr <- coin::logrank_test(Surv(y, event) ~ factor(tr==tl))
  } else {
    lr <- coin::logrank_test(Surv(y, event) ~ factor(tr==tl) | factor(block))
  }
  return(coin::statistic(lr, "test"))
}


honor.p <- NPC(data=honor.dta, tr.var="Southern", tr.label=1,
               y.vars=c("UseOfForce", "Duration", "Outcome"),
               block.var="Pair", clust.var="President", event.var="Event",
               n.perms=9999, alternative = "greater", seed=1,
               comb.fun="NormalCF",
               test.statistic=c("HarmonicWtdMean", "logrank", "KSless"),
               na.rm=TRUE, FWE.adj=TRUE, step.down=FALSE)
round(honor.p$p.values, 2)



################################################################################
##### CAUGHEY (2012) ###########################################################
################################################################################

TVA.dta <- read.dta("Data/TVA.dta")
TVA.dta <- plyr::mutate(TVA.dta,
                        DifConIRTAve52 = ConIRTAve52 - ConIRT30,
                        DifConIRTAve54 = ConIRTAve54 - ConIRT30,
                        DifConIRTAve56 = ConIRTAve56 - ConIRT30,
                        DifConIRTAve58 = ConIRTAve58 - ConIRT30,
                        DifConIRTAve60 = ConIRTAve60 - ConIRT30)
summary(TVA.dta)

## Correlations
tva.y.vars <- names(subset(TVA.dta,, ConIRTAve52:ConIRTAve60))
round(cov2cor(cov(subset(TVA.dta, TVA=="TVA", tva.y.vars))), 2)
round(cov2cor(cov(subset(TVA.dta, TVA=="Non-TVA", tva.y.vars))), 2)

## Raw post-treatment differences
TVA.mean <- NPC(data=TVA.dta, tr.var="TVA", tr.label="TVA", na.rm=TRUE,
                y.vars=tva.y.vars, block.var="Pair", n.perms=9999,
                alternative="less", seed=1, comb.fun="ProductCF",
                test.statistic="StudentsT", FWE.adj=TRUE, step.down=FALSE)
round(as.matrix(TVA.mean$p.values), 2)

TVA.rank <- NPC(data=TVA.dta, tr.var="TVA", tr.label="TVA", na.rm=TRUE,
                y.vars=tva.y.vars, block.var="Pair", n.perms=9999,
                alternative="less", seed=1, comb.fun="ProductCF", 
                test.statistic="StudentWilcoxon", FWE.adj=TRUE,
                step.down=FALSE)
round(as.matrix(TVA.rank$p.values), 2)

## Combining both sets of tests
NPC(data=TVA.dta, tr.var="TVA", tr.label="TVA", y.vars=rep(tva.y.vars, 2),
    block.var="Pair", n.perms=9999, alternative="less", seed=1,
    comb.fun="ProductCF",
    test.statistic=c(rep("StudentWilcoxon", 5), rep("StudentsT", 5)),
    FWE.adj=TRUE, step.down=FALSE)


################################################################################
#### CAUGHEY & SEKHON (2011) ###################################################
################################################################################

library(RItools)

## read in data
rd.dta <- read.dta('Data/RDReplication140410.dta')
summary(rd.dta)

## covariates on which to test balance
covs <- c('DWinPrv', 'DPctPrv', 'DifDPPrv', 'IncDWNOM1', 'DemInc', 'NonDInc',
          'PrvTrmsD', 'PrvTrmsO', 'RExpAdv', 'DExpAdv', 'ElcSwing', 'CQRating3',
          'DSpndPct', 'DDonaPct', 'SoSDem', 'GovDem', 'DifPVDec', 'DemOpen',
          'NonDOpen', 'OpenSeat', 'VtTotPct', 'GovWkPct', 'UrbanPct',
          'BlackPct', 'ForgnPct')
## subset within which to evaluate covariate balance
within.5 <- rd.dta$Use == 1 & abs(rd.dta$DifDPct) < .5 & !is.na(rd.dta$DifDPct)
no.miss.covs <- rowSums(is.na(rd.dta[covs])) == 0
sum(within.5 & no.miss.covs)
summary(rd.dta[within.5 & no.miss.covs, covs])

(cov.lev <- sapply(covs, function (cov) nlevels(factor(rd.dta[, cov]))))
(cov.signs <- sapply(covs, function (cov) {
    tr <- rd.dta$DemWin == 1
    s <- sign(median(rd.dta[within.5 & no.miss.covs & tr, cov]) -
              median(rd.dta[within.5 & no.miss.covs & !tr, cov]))
    ifelse(s > 0, 1, -1)
}))
cov.names <- c("Dem Win t - 1", "Dem % t - 1", "Dem % Margin t - 1",
               "Inc's D1 NOMINATE", "Dem Inc in Race", "Rep Inc in Race",
               "Dem's # Prev Terms", "Rep's # Prev Terms", "Rep Experience Adv",
               "Dem Experience Adv", "Partisan Swing", "CQ Rating {-1, 0, 1}",
               "Dem Spending %", "Dem Donation %", "Dem Sec of State",
               "Dem Governor", "Dem Pres % Margin", "Dem-held Open Seat",
               "Rep-held Open Seat", "Open Seat", "Voter Turnout %",
               "Pct Gov't Worker", "Pct Urban", "Pct Black", "Pct Foreign Born") 

### Listwise deletion of observations with missing values (N=22)
## Reproduction of Figure 2 in Caughey & Sekhon (2011)
rd.npc1 <- NPC(data=rd.dta[within.5 & no.miss.covs, ],
               tr.var='DemWin', tr.label=1, y.vars=covs,
               n.perms=9999, alternative = "two.sided",
               seed=1, comb.fun='ProductCF',
               FWE.adj=TRUE, step.down=TRUE, return.matrix=FALSE,
               test.statistic=ifelse(cov.lev == 2,
                   'DiffMeans', 'RankSum'))
rd.npc1$p.values
gg.df1 <- data.frame(Covariate=c(rep(covs, 2), 'NPC'),
                    FWE=grepl('adj', names(rd.npc1$p.values)),
                    p=rd.npc1$p.values)
gg.df1

rd.plot <- ggplot(subset(gg.df1, Covariate != 'NPC'),
                  aes(x=p, y=factor(Covariate, rev(c('NPC', covs)),
                               labels=rev(c('NPC', cov.names))))) +
    geom_point(aes(shape=FWE), size=4) +
    labs(x=paste('Two-Sided Balance p-Value (Square-Root Scale)'),
         y=element_blank()) +
    scale_shape_manual(labels=c('Raw', 'Adjusted'),
                         guide = guide_legend(title=NULL),
                         values=c(20, 1)) +
    scale_x_sqrt(breaks=c(.0025, 0.01, 0.05, .1, .2, .5, 1),
                 labels=c('.0025', '.01', '.05', '.1', '.2', '.5', '1'),
                 lim=c(0, 1)) +
    geom_vline(xintercept=.05, linetype='dotted') +
    theme_bw() +
    theme(legend.position="bottom") +
    ggtitle(paste('Covariate Balance in a 0.5% Window,',
                  'Non-Missing Observations (N = 22)\n',
                  'NPC Global p =', round(rd.npc1$p.values['NPC'], 3)))

rd.plot
ggsave('PDF/RDBalanceLD.pdf', width=9, height=5.5)

## Same covariates but using Kolmogorov-Smirnov statistic
rd.npc2 <- NPC(data=rd.dta[within.5 & no.miss.covs, ],
               tr.var='DemWin', tr.label=1, y.vars=covs,
               n.perms=10000, alternative = "two.sided",
               seed=1, comb.fun='ProductCF',
               na.rm=TRUE, FWE.adj=TRUE, step.down=TRUE,
               test.statistic='KS')
rd.npc2$p.values

### Keep all obs. (N=85) and use test statistics designed for missing data
## Test observed responses only, assuming data are MCAR
rd.npc3 <- NPC(data=rd.dta[within.5, ],
               tr.var='DemWin', tr.label=1, y.vars=covs,
               n.perms=10000, alternative = "two.sided",
               seed=1, comb.fun='ProductCF',
               na.rm=FALSE, FWE.adj=TRUE, step.down=TRUE,
               return.matrix = TRUE,
               test.statistic=ifelse(cov.lev == 2,
                   'DiffSumWithNA', 'RankSumWithNA')
               ) 
rd.npc3$p.values

gg.df3 <- data.frame(Covariate=c(rep(covs, 2), 'NPC'),
                    FWE=grepl('adj', names(rd.npc3$p.values)),
                    p=rd.npc3$p.values)
gg.df3

sum(gg.df3[26:50, 'p'] < .1)
p.adjust(sort(gg.df3[covs, 'p']), method='holm')

rd.plot %+% subset(gg.df3, Covariate != 'NPC') +
    ggtitle(paste('Covariate Balance in a 0.5% Window,',
                  'All Observations (N = 85)\n',
                  'NPC Global p < 0.001'))
ggsave('PDF/RDBalanceObs.pdf', width=9, height=5.5)


## Test both observed responses and pattern of missingness
ts.miss <- NULL
for (i in seq_along(covs)) {
    if (cov.lev[i] == 2) {
        ts.miss <- c(ts.miss, c('DiffSumWithNA', 'DiffSumObs'))
    }
    else {
        ts.miss <- c(ts.miss, c('RankSumWithNA', 'DiffSumObs'))
    }
}
ts.miss
rd.npc4 <- NPC(data=rd.dta[within.5, ],
               tr.var='DemWin', tr.label=1, y.vars=rep(covs, each=2),
               n.perms=9999, alternative = "two.sided", seed=1,
               comb.fun='ProductCF',
               na.rm=FALSE, FWE.adj=TRUE, step.down=TRUE,
               test.statistic=ts.miss)
names(rd.npc4$p.values) <-
    gsub('.1', ' NA', names(rd.npc4$p.values), fixed=TRUE)
rd.npc4$p.values

gg.df4 <- data.frame(Covariate=c(rep(rep(covs, each=2), 2), 'NPC'),
                    Missing=grepl('NA', names(rd.npc4$p.values)),
                    FWE=grepl('adj', names(rd.npc4$p.values)),
                    p=rd.npc4$p.values)
gg.df4

ggplot(subset(gg.df4, FWE),
       aes(x=p, y=factor(Covariate, rev(c('NPC', covs)),
                    labels=rev(c('NPC', cov.names))))) +
    geom_point(aes(shape=Missing)) +
    labs(x='FWER-Adjusted Balance p-Value (Square-Root Scale)',
         y=element_blank()) +
    scale_shape_manual(labels=c('Observed Responses', 'Missingness Indicator'),
                       guide = guide_legend(title=NULL), values=c(1, 4)) +
    scale_x_sqrt(breaks=c(0.0025, 0.01, 0.05, .1, .2, .5, 1),
                 labels=c('.0025', '.01', '.05', '.1', '.2', '.5', '1'),
                 lim=c(0, 1)) +
    geom_vline(xintercept=.05, linetype='dotted') +
    theme_bw() +
    theme(legend.position="bottom") +
    ggtitle('Covariate Balance in a 0.5% Window, All Observations (N=85)
NPC Global p < 0.0001')
ggsave('PDF/RDBalanceObsNA.pdf', width=9, height=5)

### Comparison with xBalance
## Listwise deletion
xBalance(fmla=as.formula(paste("DemWin ~ ", paste(covs, collapse= "+"))),
         data=rd.dta[within.5, ], report='all', na.rm=TRUE)

## Imputing NAs with median
xBalance(fmla=as.formula(paste("DemWin ~ ", paste(covs, collapse= "+"))),
         data=rd.dta[within.5, ], report='all', na.rm=FALSE, impfn=median)

################################################################################
##### FEARON, HUMPHREYS & WEINSTEIN (2009) #####################################
################################################################################

## Load Fearon et al. (2009) replication data
cdr.dta <- read.dta("Data/aea_replicate.dta")
summary(cdr.dta)

## Hypotheses
## H1: Larger share of available funds earned (PAYOUT)
## H2: Larger average share of 300LD contributed (AVERAGE)
## H3: Larger share contributing full amount (MEANMAX)

## Correlations
cov2cor(cov(subset(cdr.dta,, AVERAGE:PAYOUT)))
cov2cor(cov(subset(cdr.dta, TREATMENT=="Treatment", AVERAGE:PAYOUT)))
cov2cor(cov(subset(cdr.dta, TREATMENT=="Control", AVERAGE:PAYOUT)))

## p-values for matching estimates (Table 1)
dt(6.5/2.6, 41) ## PAYOUT
dt(5.7/2.6, 41) ## AVERAGE
dt(9.1/3.7, 41) ## MEANMAX
## T-tests
t.test(PAYOUT ~ TREATMENT, cdr.dta, alternative = "less")
t.test(AVERAGE ~ TREATMENT, cdr.dta, alternative = "less")
t.test(MEANMAX ~ TREATMENT, cdr.dta, alternative = "less")
## Bivariate regression  estimates
robcov(ols(PAYOUT ~ TREATMENT, cdr.dta, x=TRUE))
robcov(ols(AVERAGE ~ TREATMENT, cdr.dta, x=TRUE))
robcov(ols(MEANMAX ~ TREATMENT, cdr.dta, x=TRUE))
## Interaction with WOMEN treatment (heterogeneity)
robcov(ols(PAYOUT ~ TREATMENT * WOMEN, cdr.dta, x=TRUE))
robcov(ols(AVERAGE ~ TREATMENT * WOMEN, cdr.dta, x=TRUE))
robcov(ols(MEANMAX ~ TREATMENT * WOMEN, cdr.dta, x=TRUE))

cdr.y.vars <- c("PAYOUT", "AVERAGE", "MEANMAX")

## NPC analysis
cdr.p <- NPC(data=cdr.dta, tr.var="TREATMENT", tr.label="Treatment",
             ## block.var="WOMEN", 
             y.vars=cdr.y.vars, n.perms=9999, alternative = "greater",
             seed=1, comb.fun="ProductCF", test.statistic="StudentsT",
             na.rm=TRUE, FWE.adj=FALSE)
round(cdr.p$p.values, 2)

## MANOVA
Anova(lm(cbind(PAYOUT, AVERAGE, MEANMAX) ~ TREATMENT, data=cdr.dta))
Anova(lm(cbind(PAYOUT, AVERAGE, MEANMAX) ~ (TREATMENT + WOMEN)^2,
         data=cdr.dta))
independence_test(PAYOUT + AVERAGE + MEANMAX ~ factor(TREATMENT) | WOMEN,
                  data=cdr.dta, distribution = approximate(10000),
                  teststat = "quad")

