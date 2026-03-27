# !diagnostics off
rm(list=ls())
options(scipen=999)
options(java.parameters = "-Xmx4g")

#===============================================================================
#  File:     Groemping_Halpin_2021_PolicSciences_replication.R
#  Date:     13 August 2021
#  Paper:    Do think tanks generate media attention on issues they care about? 
#            Mediating internal expertise and prevailing governmental agendas
#  Journal:  Policy Sciences
#  Authors:  Max Groemping, Darren R. Halpin,
#  Purpose:  Replicate data analysis and all tables and figures of the article
#===============================================================================

# LOAD PACKAGES ---------------------------------------------------------------
require(lme4)
require(ggplot2)
require(tidyverse)
require(dplyr)
require(bbmle)
require(texreg)
require(performance)
require(glmmADMB)
require(glmmTMB)
require(reshape2)
require(sjPlot)

setwd("--- your directory ---")


# LOAD DATA ---------------------------------------------------------------
load("dat.RData")  # Data frame containing all model variables. The replication 
                   # code for the textual analysis for policy topic and mentions 
                   # conducted on proprietary Factiva corpus with quanteda package 
                   # is available from from authors upon request
dfm <- read.csv("dfm_research.csv")   

# Figure 1. Distribution of media attention across Australian thin --------
f1 <- merge %>%
  group_by(name) %>%
  summarise(Mentions_tot = sum(Mentions),
                   Mentions_perc = Mentions_tot/15501) %>%
  ungroup() %>%
  as.data.frame()

ggplot(f1, aes(x = reorder(name, -Mentions_perc), y = Mentions_perc)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x = "Count", 
       y = "Percent of Total Media Mentions", 
       title = "")

ggsave(filename = "fig 1.jpg", width = 5, height = 4, dpi = 600, units = "in",  device = "jpg")



# Table 1. Distribution of policy contexts across media mentions  ---------
t1 <- merge %>%
  group_by(Topic, name) %>%
  summarise(TT_mentioned = ifelse(sum(Mentions)>0, 1, NA)) %>%
  filter(!is.na(TT_mentioned)) %>%
  ungroup() %>%
  as.data.frame()

t1 <- t1 %>%
  group_by(name) %>%
  summarise(contexts = ifelse(sum(TT_mentioned)==1, "1",
                              ifelse(sum(TT_mentioned)>1 & sum(TT_mentioned)<6, "2-5",
                                     ifelse(sum(TT_mentioned)>5 & sum(TT_mentioned)<11, "6-10",
                                            ifelse(sum(TT_mentioned)>10 & sum(TT_mentioned)<16, "11-15",
                                                   ifelse(sum(TT_mentioned)>15 & sum(TT_mentioned)<19, "16-18",
                                                          "19")))))) %>%
  ungroup() %>%
  as.data.frame()

t1 <- table(ordered(t1$contexts, levels = c("1","2-5","6-10","11-15","16-18","19")))
write.csv(cbind(t1, prop.table(t1)), file="table 1.csv")





# Figure A1: Breadth of think tanks' policy focus -------------------------
fa1 <- merge %>%
  group_by(name) %>%
  summarise(TT_focus = sum(ifelse(Interest=="Policy Focus", 1, 0))) %>%
  ungroup() %>%
  as.data.frame()

ggplot(fa1, aes(TT_focus)) +
  geom_histogram(binwidth =1,  colour = "black") +
  scale_x_continuous(breaks = seq(0, 9, by = 1))+
  theme_classic() +
  labs(x="Breadth of Policy Interests (no. of topics)", y = "No. of Think Tanks")

ggsave(filename = "fig A1.jpg", width = 5, height = 4, dpi = 600, units = "in",  device = "jpg")


# Table A2: Distribution of think tanks' policy focus  ------------------------
fa2 <- as.data.frame(cbind(table(merge$Topic, merge$Interest), prop.table(table(merge$Topic, merge$Interest), margin = 1)))[,c(2,4)]
write.csv((fa2)[order(-fa2$`Policy Focus.1`),], file="table A2.csv")






# MODELS ------------------------------------------------------------------
# create model terms
form1  <- mention   ~ Interest + govt_agenda1 + TYPE + log_staff + Media + Ideology + Policy_Scope + ACT_HQ + age + (1|namefact) # binary outcome
form2  <- Mentions  ~ Interest + govt_agenda1 + TYPE + log_staff + Media + Ideology + Policy_Scope + ACT_HQ + age + (1|namefact) # count outcome
zi     <-           ~ Interest + govt_agenda1 + TYPE + log_staff + Media + Ideology + Policy_Scope + ACT_HQ + age + (1|namefact) # hurdle/zero component

# test for overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(glmer(form2, data = merge, family = "poisson"))          # the data are overdispersed (X2 (1163, N = 1178) = 23429, p < .001.)

# run models
icc(M0 <- lme4::glmer(mention  ~ 1 + (1 | name),  data = merge, family=binomial(link="logit")))  # Null model ( multilevel warranted, since ICC=0.566)
M1     <- glmmadmb(form1, data=merge, zeroInflation=F, family="binomial") 
M2null <- lme4::glmer(Mentions  ~ 1 + (1 | namefact), data = merge, family = "poisson")
M2p    <- glmmadmb(form2, data=merge, zeroInflation=F, family="poisson")
M2nb   <- glmmadmb(form2, data=merge, zeroInflation=F, family="nbinom1")
M2zip  <- glmmadmb(form2, data=merge, zeroInflation=T, family="poisson")
M2zinb <- glmmadmb(form2, data=merge, zeroInflation=T, family="nbinom1")
M2hp   <- glmmTMB(form2, zi = zi, merge, family=truncated_poisson, control = glmmTMBControl(optCtrl=list(iter.max=1e9,eval.max=1e9)))
M2hnb  <- glmmTMB(form2, zi = zi, merge, family=truncated_nbinom2, control = glmmTMBControl(optCtrl=list(iter.max=1e9,eval.max=1e9)))

# AIC comparison  
bbmle::AICtab(M2p, M2nb, M2zip, M2zinb, M2hp, M2hnb)  # M2hnb has best AIC

# Table A3. Comparing count models explaining media attention to t --------
htmlreg((list(M2null,M2p, M2nb, M2zip, M2zinb, M2hp, M2hnb)),
        custom.model.names = c("Null","Poiss", "NB", "ZI-Poiss", "ZI-NB", "Hurdle-Poiss", "Hurdle-NB"),
        single.row=TRUE,
        bold = 0.05,
        stars = c(0.01, 0.05, 0.1),
        leading.zero = F,
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
        head.tag = TRUE, body.tag = TRUE,
        file="table A3.doc")


# Figure A.3 Observed versus predicted counts -----------------------------
merge$M2p <- predict(M2p,type="response")
merge$M2nb <- predict(M2nb,type="response")
merge$M2zip <- predict(M2zip,type="response")
merge$M2zinb <- predict(M2zinb,type="response")
merge$M2hp <- predict(M2hp,type="response")
merge$M2hnb <- predict(M2hnb,type="response")

pred <- merge[c("Mentions", "M2p", "M2nb", "M2zip", "M2zinb", "M2hp", "M2hnb")]
pred <- plyr::rename(pred, c("Mentions" = "Observed",
                             "M2p" = "Poiss", 
                             "M2nb" = "NB", 
                             "M2zip" = "ZI-Poiss", 
                             "M2zinb" = "ZI-NB", 
                             "M2hp" = "Hurdle-Poiss", 
                             "M2hnb" = "Hurdle-NB"))
pred <- reshape2::melt(pred)
pred <- plyr::rename(pred, c("variable" = "Model"))

scaleFUN <- function(x) sprintf("%.2f", x)

ggplot(pred, aes(value, linetype = Model, size = Model, alpha = Model)) +
  geom_freqpoly(binwidth = 1) +
  scale_size_manual(values = c(0.6, 0.3, 0.3,0.3,0.3,0.3,0.6)) +
  scale_alpha_manual(values = c(1, 0.5, 0.5,0.5,0.5,0.5,1)) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash", "dashed","twodash",  "4C88C488", "dotted")) +
  xlim(0,10) +
  labs(x = "Frequency", y = "Count") +
  theme_classic()

ggsave(filename = "fig A3.jpg", width = 4, height = 2, dpi = 600, units = "in",  device = "jpg")






# Table 2: Explaining media attention to think tanks ----------------------
# report final models
htmlreg((list(M1, M2hnb)),
        custom.model.names = c("M1: Access","M2: Mentions"),
        single.row=TRUE,
        bold = 0.05,
        stars = c(0.01, 0.05, 0.1),
        leading.zero = F,
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
        head.tag = TRUE, body.tag = TRUE,
        file="table 2.doc")



# Figure 3: Predicting media attention to think tanks ---------------------
plot_model(M2hnb, type = "emm", terms = c("govt_agenda1[all]", "Interest"), ci.lvl = 0.90, colors = "bw") +
  geom_rug(alpha = 0.2, position = position_jitter(w = 0.1, h = 0.1)) + 
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  labs(x = "Percent. of govt. agenda on policy topic", 
       y = "Mentions (predicted count on policy topic)", 
       title = "")

ggsave(filename = "fig 3.jpg", width = 5, height = 4, dpi = 600, units = "in",  device = "jpg")







# Figure 2. Proportion of Think Tank Media Mentions on basis of Re --------
dfm <- dfm[order(dfm$Percent),]
dfm$Group_name[dfm$Group_name=="Committee for Economic Development of Australia"] <- "C. for Econ. Development of Austr."

ggplot(dfm, aes(x = reorder(Group_name, -Percent), y = Percent)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1), axis.title.y = element_blank())+
  theme_classic() +
  coord_flip() +
  labs(y = "Percent of mentions in conjuntion with research", 
       x = "Think tank name (top 20 only)", 
       title = "")
ggsave(filename = "fig 2.jpg", width = 6, height = 4, dpi = 600, units = "in",  device = "jpg")

