##########################################################################
# Figure 2: Effects of Campaign Spending on Republicanos’ Mayoral Candidacies and Victories
##########################################################################
###

rm(list=ls())
library(data.table)
library(ggplot2)
library(rdrobust)
library(ggthemes)
library(ggplot2)
library(here)

electoral <- readRDS("data/mayors_municipal_level.rds")

# LEFT

party_test <- data.frame(conventional_coef=as.numeric(),
                           robust_coef = as.numeric(),
                           conventional_upper = as.numeric(),
                           conventional_lower = as.numeric(),
                           robust_upper = as.numeric(),
                           robust_lower = as.numeric(),
                           n=as.numeric(),
                           group=as.character(),
                           dv=as.character(),
                           stringsAsFactors=FALSE)


prb_candidate <- with(electoral, rdrobust(prb_candidate_2016, inverted_margin, all=TRUE))
summary(prb_candidate)



temp <- data.frame(conventional_coef = prb_candidate$coef[[1]],
                   robust_coef = prb_candidate$coef[[3]],
                   conventional_upper = prb_candidate$ci[[4]],
                   conventional_lower = prb_candidate$ci[[1]],
                   robust_upper = prb_candidate$ci[[6]],
                   robust_lower = prb_candidate$ci[[3]],
                   n = prb_candidate$N_h[[1]] + prb_candidate$N_h[[2]],
                   group = 'After Spending Caps',
                   dv = 'Fielded Candidate')

party_test = rbind(party_test,temp)

prb_elected <- with(electoral, rdrobust(prb_elected_2016, inverted_margin, all=TRUE))
summary(prb_elected)

temp <- data.frame(conventional_coef = prb_elected$coef[[1]],
                   robust_coef = prb_elected$coef[[3]],
                   conventional_upper = prb_elected$ci[[4]],
                   conventional_lower = prb_elected$ci[[1]],
                   robust_upper = prb_elected$ci[[6]],
                   robust_lower = prb_elected$ci[[3]],
                   n = prb_elected$N_h[[1]] + prb_elected$N_h[[2]],
                   group = 'After Spending Caps',
                   dv = 'Elected Mayor')
party_test = rbind(party_test,temp)

# 2012

prb_candidate_2012 <- with(electoral, rdrobust(prb_candidate_2012, inverted_margin, all=TRUE))
summary(prb_candidate_2012)

temp <- data.frame(conventional_coef = prb_candidate_2012$coef[[1]],
                   robust_coef = prb_candidate_2012$coef[[3]],
                   conventional_upper = prb_candidate_2012$ci[[4]],
                   conventional_lower = prb_candidate_2012$ci[[1]],
                   robust_upper = prb_candidate_2012$ci[[6]],
                   robust_lower = prb_candidate_2012$ci[[3]],
                   n = prb_candidate_2012$N_h[[1]] + prb_candidate_2012$N_h[[2]],
                   group = 'Before Spending Caps',
                   dv = 'Fielded Candidate')

party_test = rbind(party_test,temp)

prb_elected_2012 <- with(electoral, rdrobust(prb_elected_2012, inverted_margin, all=TRUE))
summary(prb_elected_2012)

temp <- data.frame(conventional_coef = prb_elected_2012$coef[[1]],
                   robust_coef = prb_elected_2012$coef[[3]],
                   conventional_upper = prb_elected_2012$ci[[4]],
                   conventional_lower = prb_elected_2012$ci[[1]],
                   robust_upper = prb_elected_2012$ci[[6]],
                   robust_lower = prb_elected_2012$ci[[3]],
                   n = prb_elected_2012$N_h[[1]] + prb_elected_2012$N_h[[2]],
                   group = 'Before Spending Caps',
                   dv = 'Elected Mayor')

party_test = rbind(party_test,temp)

party_test$group <- factor(party_test$group,levels = c("Before Spending Caps","After Spending Caps"))
party_test$dv <- factor(party_test$dv,levels = c('Fielded Candidate','Elected Mayor'))

# Plot ----
plot.candidates <- ggplot(party_test, aes(x = dv, y = robust_coef, group = group, shape = group, color = group)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0, linewidth =.75,position=position_dodge(width=0.5)) +
  geom_point(alpha=.9,size=2.5 ,position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  scale_color_manual(values=c("grey", "black")) +
  theme(legend.position = "top") +
  theme(legend.title=element_blank()) +
  xlab("") +
  ylab('Effect of Stricter Spending Caps,\n Republicanos') +
  NULL

#ggsave(here('img','Fig_2_left.pdf'), plot = plot.candidates, device = 'pdf',height = 13, width = 13, units = 'cm') 


# RIGHT PANEL ----
electoral[, treat := ifelse(inverted_margin > 0, 1, 0)]


sequence=seq(10000, 100000, 10000)
minimal_obs = 250

electoral[, dv := prb_candidate_2016]

dom <- data.frame(coef=as.numeric(),
                  SD=as.numeric(),
                  n=as.numeric(),
                  stringsAsFactors=FALSE)

for(i in sequence){
  rd.dom <- electoral[abs(electoral$inverted_margin) <= i]
  domreg <- with(rd.dom, lm(I(dv) ~ treat*inverted_margin))
  domreg.SE <- sqrt(diag(vcovHC(domreg, type = "HC3")))
  tempo.a <- data.frame(coef = domreg$coefficients[2], SD = domreg.SE[2], n = domreg$df)
  dom <- rbind(dom,tempo.a)   
}

dom$cuts <- sequence
dom$confidence <- with(dom, qt(.975, df = n - 1) * SD)
dom$upper <- with(dom, coef + confidence)
dom$lower <- with(dom, coef - confidence)

dom$description <- 'Local Linear'

data.plot <- data.table(rbind(dom))

plot.mayorsPRB <- ggplot(data.plot[n>=minimal_obs], aes(x = cuts, y = coef, group = description,color=description,shape=description)) +
  geom_errorbar(data=data.plot[n>=minimal_obs],aes(ymax = (upper), ymin = (lower)), width=0, linewidth=.75,position=position_dodge(width=0.9)) +
  geom_point(alpha=.9,size=2.5) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 14) +
  scale_colour_grey() +
  theme(legend.position = "none") +
  xlab("Distance from Cutoff") +
  ylab('Probability of Republicanos Mayoral Candidate') +
  geom_text(data=data.plot[cuts %in% c(min(data.plot[n>=minimal_obs]$cuts),seq(min(data.plot[n>=minimal_obs]$cuts),100000,20000))],aes(y = .225,label = n),vjust=-.50,size=4,col='gray40') +
  coord_cartesian(xlim=c(min(data.plot[n>=minimal_obs]$cuts),100000)) +
  geom_segment(aes(x = 10000, xend = 100000, y = 0.225, yend = 0.225), size=.05) +
  annotate('text', y = 0.22, x=90000, label = '(n. municipalities)',size=3,col='gray40',fontface = 'italic') +
  NULL


#ggsave(here("img","Fig_2_right.pdf"), plot = plot.mayorsPRB, device = 'pdf',height = 13, width = 13, units = 'cm') 
