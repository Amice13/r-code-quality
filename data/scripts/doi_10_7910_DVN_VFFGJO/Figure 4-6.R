############################################################################################################################################
## Replication Data for:                                                                                                                   #
## Wawro, Gregory, and Ira Katznelson. Time Counts: Quantitative Analysis for Historical Social Science. Princeton University Press, 2022. #
############################################################################################################################################
## Figure in the book is generated with an older version of MCMCpack (Version 1.2-2). Using a recent version (e.g., 1.6-3) results in 
## different posterior probabilities. See _README.txt.

rm(list = ls())
library(MCMCpack)
library(foreign)
library(ggplot2)
gjdat     <- read.dta("../Data/GailmardJenkins.dta")
gjdat_del <- subset(gjdat, year>=1880 & year<=1940 & nomiss != 0 & state != "" & jcr_d != "NA" & time != "NA")
gjdat_del <- transform(gjdat_del, time = time - 2)

FEols     <- lm(lnsmaxdistnew ~ tpr + tpr2 + jcr_d + jcr_d2 + time + elect_str_both + as.factor(state) - 1, data = gjdat_del)
resid.all <- rstandard(FEols)

## Model fitting
G  <- 1000
BF <- testpanelSubjectBreak(subject.id = gjdat_del$state,
                            time.id    = gjdat_del$time,
                            resid      = resid.all, 
                            max.break  = 1, 
                            minimum    = 0,
                            mcmc       = G, 
                            burnin     = G, 
                            thin       = 1, 
                            verbose    = G, 
                            b0         = 0, 
                            B0         = 1/100, 
                            c0         = 2, 
                            d0         = 2,
                            Time       = gjdat_del$time)
estimated.breaks <- make.breaklist(BF, thresh=1)

## Note that the code below was executed with an older version of MCMCpack. Using a current version of MCMCpack may cause errors. See _README.txt. 
# xvar <- cbind(gjdat_del$tpr, gjdat_del$tpr2, gjdat_del$jcr_d, gjdat_del$jcr_d2, gjdat_del$time, gjdat_del$elect_str_both)
# out  <- HMMpanelFE(subject.id = gjdat_del$state,
#                    y = gjdat_del$lnsmaxdist,
#                    X = xvar,
#                    m = estimated.breaks,
#                    mcmc    = G,
#                    burnin  = G,
#                    thin    = 1,
#                    verbose = G,
#                    b0 = 0,
#                    B0 = 1/1000,
#                    c0 = 2,
#                    d0 = 2,
#                    delta0 = 0,
#                    Delta0 = 1/1000)

p  <- attr(BF, "model.prob")[,1]

p.df         <-  data.frame(matrix(p, 48, 2, byrow=T))
names(p.df)  <- c("statenum","post.probs")
alphast      <- unique(gjdat_del$alphast)
stateseq     <- seq(1,48,1)
forplot      <- cbind.data.frame(data.frame(alphast),p.df)
forplot1     <- cbind(forplot[order(forplot$alphast),],stateseq)

pp  <- ggplot(forplot1, aes(y = alphast, x = post.probs)) +
  geom_segment(aes(y = alphast, yend = alphast, x = 0.5, xend = post.probs), color = "black") +
  geom_point(color = "black", size = 3) +
  theme_light() +
  theme(
    text = element_text(size = 14),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y  = element_text(size = 11, color = "black"),
    axis.text.x  = element_text(size = 13, color = "black"),
    axis.title.y = element_text(size = 15)
  ) +
  scale_x_continuous(limits = c(.5,1.01), expand = c(0,0)) +
  xlab("\n Posterior Probability of No Structural Break") +
  ylab("State")

ggsave(pp, filename="../Figures/Figure 4-6.pdf", height = 9, units = "in")

