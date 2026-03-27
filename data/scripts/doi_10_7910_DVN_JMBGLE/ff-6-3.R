require(VGAM)
require(lfe)
require(dplyr)
require(ggplot2)

load('news_data_wContext.Rda')

news$pres_v_cong <- news$n_pres_near_attrib-news$n_cong_near_attrib
news$lede_pres_v_cong <- news$lede_n_pres_near_attrib-news$lede_n_cong_near_attrib

# Results In-text: Are executive action articles more likely to attribute policy change to the president?

# simple t.tests -- YES
t.test(n_pres_near_attrib~executive_action,news,na.rm=T)
t.test(lede_n_pres_near_attrib~executive_action,news,na.rm=T)

# simple t.tests w/ log transformations -- YES
t.test(log(n_pres_near_attrib+1)~executive_action,news,na.rm=T)
t.test(log(lede_n_pres_near_attrib+1)~executive_action,news,na.rm=T)

# tobit regression -- YES
summary(vglm(n_pres_near_attrib ~ executive_action, tobit(Lower = 0), data = news))
summary(vglm(n_pres_near_attrib ~ pres_in_office + pub_title + n_words + executive_action, tobit(Lower = 0), data = news))

summary(vglm(lede_n_pres_near_attrib ~ executive_action, tobit(Lower = 0), data = news))
summary(vglm(lede_n_pres_near_attrib ~ pres_in_office + executive_action, tobit(Lower = 0), data = news))

# OLS with transformed variables  -- YES
summary(lm(log(n_pres_near_attrib+1) ~ executive_action, data = news))
summary(lm(log(n_pres_near_attrib+1) ~ pres_in_office + pub_title + n_words + executive_action, data = news))

summary(lm(log(lede_n_pres_near_attrib+1) ~ executive_action, data = news))
summary(lm(log(lede_n_pres_near_attrib+1) ~ pres_in_office + pub_title + n_words + executive_action, data = news))

# Are executive action articles less likely to attribute gridlock to the president?

# simple t.tests -- Yes overall, less certain for the lede
t.test(n_pres_near_grdlck~executive_action,news,na.rm=T)
t.test(lede_n_pres_near_grdlck~executive_action,news,na.rm=T)

# tobit regression -- Yes bivariate, but not with controls, and not in the lede.
summary(vglm(n_pres_near_grdlck ~ executive_action, tobit(Lower = 0), data = news))
summary(vglm(n_pres_near_grdlck ~ pres_in_office +  n_words + executive_action, tobit(Lower = 0), data = news))

summary(vglm(lede_n_pres_near_grdlck ~ executive_action, tobit(Lower = 0), data = news))
summary(vglm(lede_n_pres_near_grdlck ~ pres_in_office + executive_action, tobit(Lower = 0), data = news))

# OLS with transformed variables  -- Yes bivariate, not with controls, and not in the lede
summary(lm(log(n_pres_near_grdlck+1) ~ executive_action, data = news))
summary(lm(log(n_pres_near_grdlck+1) ~ pres_in_office + pub_title + n_words +  executive_action, data = news))

summary(lm(log(lede_n_pres_near_grdlck+1) ~ executive_action, data = news))
summary(lm(log(lede_n_pres_near_grdlck+1) ~ pres_in_office + pub_title + n_words + executive_action, data = news))

# Is the amount of attribution for the president greater than the attribution given to Congress?
news$pres_v_cong <- news$n_pres_near_attrib-news$n_cong_near_attrib
news$lede_pres_v_cong <- news$lede_n_pres_near_attrib-news$lede_n_cong_near_attrib

# positive values indicate more attribution words near the president, relative to Congress

# simple t.tests -- YES
t.test(pres_v_cong~executive_action,news,na.rm=T)
t.test(lede_pres_v_cong~executive_action,news,na.rm=T)

# OLS -- YES
summary(lm(pres_v_cong ~ executive_action, data = news))
summary(lm(pres_v_cong ~ pres_in_office + pub_title + n_words + n_president + n_congress + executive_action, data = news))

summary(lm(lede_pres_v_cong ~ executive_action, data = news))
summary(lm(lede_pres_v_cong ~ pres_in_office + pub_title + n_words + executive_action, data = news))

# executive action articles more likely to lede with attribution for the president
mean(as.numeric(news$lede_n_pres_near_attrib[news$executive_action==1]>0))
mean(as.numeric(news$lede_n_pres_near_attrib[news$executive_action==0]>0))

# non-executive action articles much more likely to lede with attribution for Congress
mean(as.numeric(news$lede_n_cong_near_attrib[news$executive_action==1]>0))
mean(as.numeric(news$lede_n_cong_near_attrib[news$executive_action==0]>0))

# executive action articles are neither more or less likely to include a references to gridlock for the president
mean(as.numeric(news$lede_n_pres_near_grdlck[news$executive_action==1]>0))
mean(as.numeric(news$lede_n_pres_near_grdlck[news$executive_action==0]>0))

# non-executive action articles much less likely to lede with gridlock for Congress
mean(as.numeric(news$lede_n_cong_near_grdlck[news$executive_action==1]>0))
mean(as.numeric(news$lede_n_cong_near_grdlck[news$executive_action==0]>0))

# within article, are there more mentions of presidents and gridlock over time?

# overall, there are more attributions and more gridlock mentions
summary(grdlck.fit <- felm(log(n_pres_near_grdlck+1) ~ pub_date + n_president | lowande.uid , data = news))
summary(attrib.fit <- felm(log(n_pres_near_attrib+1) ~ pub_date + n_president | lowande.uid , data = news))

# but in the lede, there IS NOT more gridlock mentions, and there are more attributions
summary(grdlck.fit <- felm(log(lede_n_pres_near_grdlck+1) ~ pub_date + n_president | lowande.uid , data = news))
summary(attrib.fit <- felm(log(lede_n_pres_near_attrib+1) ~ pub_date + n_president| lowande.uid , data = news))
# -----------------------------------------------


# bar figures 
versus <- news[!is.na(news$pres_in_office),] %>%
  group_by(pres_in_office,executive_action) %>%
  dplyr::summarize(Mean = mean(pres_v_cong, na.rm=TRUE),n = n(), SD = sd(pres_v_cong), SE = SD/sqrt(n))

versus$pres_in_office <- factor(versus$pres_in_office,levels=c('HWBush','Clinton','Bush','Obama','Trump'))

# Figure 6.3a: Press on executive action assigns more credit to the president relative to Congress, and relative to other political news. 
bar_credit <- ggplot(versus, aes(x=pres_in_office, y=Mean, fill=pres_in_office, alpha=as.factor(executive_action))) + 
  geom_bar(stat="identity", color="black",position=position_dodge()) +
  geom_errorbar(aes(ymin=Mean-(SE*1.96), ymax=Mean+(SE*1.96)), width=.2,position=position_dodge(.9)) +
  labs(x="", y = "Relative Credit") + scale_alpha_discrete(range=c(.25,.75)) +
  geom_text(x=1.4,y=13,label='More to President',size=2.5) +
  geom_text(x=1.375,y=-10,label='More to Congress',size=2.5) +
  theme_classic() + theme(legend.title=element_blank(),legend.position='none',axis.text.x = element_text(angle = 15,colour="black"),
                          axis.text.y=element_text(colour="black")) +
  scale_fill_manual(values=c('#000000','#000000','#000000','#000000','#000000')) 

versus_l <- news[!is.na(news$pres_in_office),] %>%
  group_by(pres_in_office,executive_action) %>%
  dplyr::summarize(Mean = mean(lede_pres_v_cong, na.rm=TRUE),n = n(), SD = sd(pres_v_cong), SE = SD/sqrt(n))

versus_l$pres_in_office <- factor(versus$pres_in_office,levels=c('HWBush','Clinton','Bush','Obama','Trump'))
# -----------------------------------------------

# Figure 6.3b: Press on executive action assigns more credit to the president relative to Congress, and relative to other political news. 
bar_credit_lede <- ggplot(versus_l, aes(x=pres_in_office, y=Mean, fill=pres_in_office, alpha=as.factor(executive_action))) + 
  geom_bar(stat="identity", color="black",position=position_dodge()) +
  geom_errorbar(aes(ymin=Mean-(SE*1.96), ymax=Mean+(SE*1.96)), width=.2,position=position_dodge(.9)) +
  labs(x="", y = "Relative Credit (in lede)") + scale_alpha_discrete(range=c(.25,.75)) +
  theme_classic() + theme(legend.title=element_blank(),legend.position='none',axis.text.x = element_text(angle = 15,colour="black"),
                          axis.text.y=element_text(colour="black")) +
  scale_fill_manual(values=c('#000000','#000000','#000000','#000000','#000000')) 
# -----------------------------------------------



