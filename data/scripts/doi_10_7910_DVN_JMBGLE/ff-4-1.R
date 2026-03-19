require(ggplot2)

load('executive-actions-1989-2021.Rda')

# Results In-text: Proportion of orders that were auto-coded
mean(ua$batch_coded,na.rm=T)
# -----------------------------------------------

# Results In-text: Proportion of orders that were date-blinded
mean(ua$date_blinded)
# -----------------------------------------------

# Results In-text: Proportion of orders coded as 'minimal direct effects'
mean(ua$mde)
# -----------------------------------------------

# Results In-text: Proportion of domestic orders coded as 'minimal direct effects' (mde)
mean(ua$mde[ua$subj_foreign==0])
# -----------------------------------------------

# Results In-text: Correlation between mde and creating an organization
cor.test(ua$creates_organ,ua$mde)
# -----------------------------------------------

# Results In-text: Correlation between mde and creating an advisory organization
cor.test(ua$organ_informational,ua$mde)
# -----------------------------------------------

# Results In-text: Correlation between mde and creating an advisory organization
cor.test(ua$citations_preamble,ua$mde)
# -----------------------------------------------

# Figure 4.1: Measuring minimal direct effects 
irt_validate_1=ggplot(data=ua,aes(x=as.factor(mde),y=ff_irt1, fill=as.factor(false_front))) +
  geom_violin() + geom_jitter(width=0.1,alpha=0.7) + ylab('Indirectness (6-item)') + xlab('Minimal Direct Effects') +
  theme_bw() + scale_fill_manual(values=c("#7f7f7f", "#7f7f7f"),guide='none') + 
  theme(axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black")) + scale_x_discrete(labels=c('0'='No','1'='Yes'))
irt_validate_2=ggplot(data=ua,aes(x=as.factor(mde),y=ff_irt2, fill=as.factor(false_front))) +
  geom_violin() + geom_jitter(width=0.1,alpha=0.7) + ylab('Indirectness (4-item)') + xlab('Minimal Direct Effects') +
  theme_bw() + scale_fill_manual(values=c("#7f7f7f", "#7f7f7f"),guide='none') + 
  theme(axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black")) + scale_x_discrete(labels=c('0'='No','1'='Yes'))
# -----------------------------------------------

# Results In-text: Correlation between number of news articles and mde
t.test(n_articles~mde,data=ua)
# -----------------------------------------------

# Results In-text: Correlation between whether there was news coverage and mde
t.test((n_articles>0)~mde,data=ua)
# -----------------------------------------------

# Results In-text: Marginal effect of typical change in discretion on mde
discr_mfx <- logitmfx(mde~subj_discretion,data=ua,clustervar1='topic_consolidated')
discr_mfx$mfxest[1]*sd(ua$subj_discretion,na.rm=T)
# -----------------------------------------------

load('discretion-preambles.Rda')

# Results In-text: Marginal effect of typical change in discretion on preamble prop. for EOs/Memos
pream <- lm(preamble_prop~subj_discretion,data=validate_preamble)
sd(validate_preamble$subj_discretion)
# -----------------------------------------------

# Results In-text: mde under divided and unified government
t.test(mde~divgov,data=ua)
logitmfx(mde~divgov,data=ua,clustervar1 = 'cong')
# -----------------------------------------------

# Results In-text: preambles under divided and unified government
t.test(preamble_prop~divgov,data=validate_preamble)
validate_preamble$preamble_prop_rescaled <- rescale(validate_preamble$preamble_prop,to=c(0.001,0.999))
betamfx(preamble_prop_rescaled~divgov,data=validate_preamble,clustervar1='cong')
# -----------------------------------------------


