library(tidyverse); library(lme4); library(lmerTest); library(mgcv); library(mgcViz)

#---------------------------------------------
# reading in the data
s <- read.csv('Switchboard_LM.csv')
d <- read.csv('Main.csv')

# as the data for sentences longer than 20 is very sparse, we do the analysis on sentences shorter than 21 words
d1 <-subset(d,d$SentenceLength < 21)

d1$SubjectID = factor(d1$SubjectID)
d1$Subtype = factor(d1$Subtype)

# separating constrained and unconstrained data
C<-subset(d1,d1$Condition=='Constrained')
U<-subset(d1,d1$Condition=='Unconstrained')

# separating constrained and unconstrained data
dn<-subset(U,U$Subtype=='nfvPPA')
dl<-subset(U,U$Subtype=='lvPPA')
ds<-subset(U,U$Subtype=='svPPA')
dc<-subset(U,U$Subtype=='control')


#---------------------------------------------
#1.1 comparing controls with nfvPPA
dcn <- subset(U,U$Subtype=='control' | U$Subtype=='nfvPPA')
m1 <- lmer(data=dcn, SentenceLength ~ Subtype   + (1|SubjectID))
m2 <- lmer(data=dcn, ContentAllWordRatio ~ Subtype   + (1|SubjectID))
m3 <- lmer(data=dcn, NounVerbRatio ~ Subtype   + (1|SubjectID))
m4 <- lmer(data=dcn, HeavyVerbRatio ~ Subtype   + (1|SubjectID))
m5 <- lmer(data=dcn, GerundVerbRatio ~ Subtype   + (1|SubjectID))

#1.2 corpus analysis
# compare the 'occurrence' of word types from Switchboard corpus
t.test(s$occurrence ~ s$FunctionContent, data = s)
t.test(subset(s,s$NounVerb== 'Noun')$occurrence, subset(s,s$NounVerb== 'Verb')$occurrence )
t.test(subset(s,s$HeavyLight== 'Heavy')$occurrence, subset(s,s$HeavyLight== 'Light')$occurrence)

#1.3 comparing controls with nfvPPA
m6<-lmer(data=dcn, WordFrequencyA ~ Subtype + (1|SubjectID))
m7<-lmer(data=dcn, WordFrequencyC ~ Subtype + (1|SubjectID))

#---------------------------------------------
#section 2
dUC <- rbind(C,dc)
dUC$condition <- factor(dUC$condition)
dUC <- within(dUC, condition <- relevel(condition, ref = 'U'))
m8 <- lmer(data = dUC, FunctionAllWordRatio ~ condition + (1|SubjectID))
m9 <- lmer(data = dUC, NounVerbRatio ~ condition + (1|SubjectID))
m10 <- lmer(data = dUC, HeavyVerbRatio ~ condition + (1|SubjectID))
m11 <- lmer(data = dUC, GerundVerbRatio ~ condition + (1|SubjectID))
m12 <- lmer(data = dUC, WordFrequencyA ~ condition + (1|SubjectID))
m13 <- lmer(data = dUC, WordFrequencyC ~ condition + (1|SubjectID))

#---------------------------------------------
#section 3.1
g1 <-gam(WordFrequencyA ~ s(SentenceLength) + s(SubjectID, bs = 're'), Method = 'REML', data = dUC)
g2 <-gam(WordFrequencyA ~ s(SentenceLength) + s(SubjectID, bs = 're'), Method = 'REML', data = dc)

# create a new data frame for prediction
x0 <- seq(1, 10, by = 0.1)
x1 <- round((runif(91)*10),0)
newd <- data.frame(SentenceLength= x0 ,SubjectID= x1)

pred <- predict.gam(g1,newd)
newd[which(pred == max(pred)),]

#---------------------------------------------
#section 3.2
dnc<-rbind(dUC,dn)
dlc<-rbind(dUC,dl)
dsc<-rbind(dUC,ds)

# test each combined data set in the models below
d2 <- dnc
g31 <- gam(WordFrequencyA ~ Subtype + s(SentenceLength, by=Subtype) + s(SubjectID, bs = 're') , data = d2, Method = 'REML')
g32 <- gam(WordFrequencyA ~ s(SentenceLength) + s(SubjectID, bs = 're') , data = d2, Method = 'REML')
a <- anova(g31,g32,test='Chisq')
a

#---------------------------------------------
#section 3.3
d2 <- dsc
g41 <- gam(WordFrequencyC ~ Subtype + s(SentenceLength, by=Subtype) + s(SubjectID, bs = 're') , data = d2, Method = 'REML')
g42 <- gam(WordFrequencyC ~ s(SentenceLength) + s(SubjectID, bs = 're') , data = d2, Method = 'REML')
a <- anova(g41,g42,test='Chisq')
a

#---------------------------------------------
#section 3.4
g5 <- gam(WordFrequencyA ~ s(FunctionAllWordRatio) + s(SentenceLength) + s(SubjectID, bs = 're'), data = d1, Method = 'REML')



#_________________________________________
#figures
#_________________________________________

#Figure 1

Av = group_by(dcn, SubjectID, Subtype) %>%
  summarise(FunctionAllWordRatio = mean(FunctionAllWordRatio, na.rm=T),
            HeavyVerbRatio = mean(HeavyVerbRatio, na.rm=T),
            GerundVerbRatio = mean(GerundVerbRatio, na.rm=T),
            WordFrequencyA = mean(WordFrequencyA, na.rm=T),
            WordFrequencyC = mean(WordFrequencyC, na.rm=T),
            NounVerbRatio = mean(NounVerbRatio, na.rm=T),
            ContentAllWordRatio = mean(ContentAllWordRatio, na.rm=T))

ggplot(data = Av, aes(x = Subtype, y = ContentAllWordRatio)) +
  geom_boxplot(
    position = position_dodge(width=0.5),
    outlier.shape = 17,
    outlier.colour = 'red2',
    outlier.size = 0.1,
    aes(fill = Subtype)) +
  scale_fill_manual(values = c( '#f5f5f5', '#f5f5f5')) + # for boxplot
  #Add the scatter points (treats outliers same as 'inliers')
  geom_jitter(
    position = position_jitter(width = 0.17),
    size = 4.0,
    aes(colour = Subtype)) +
  scale_color_manual(values = c("#050d99", "#bf0404")) +
  theme_classic() +
  coord_cartesian(ylim = c(0.2, 1)) +
  theme(axis.text.x = element_text(size = 22, color = 'black'),axis.title=element_text(size=23)) +
  theme(axis.text.x = element_text(color="black", size=20, angle=0),
        axis.text.y = element_text(color="black", size=16, angle=0))
                                   

# Figure2
Av2 = group_by(dUC, SubjectID, condition) %>%
  summarise(FunctionAllWordRatio = mean(FunctionAllWordRatio, na.rm=T),
            HeavyVerbRatio = mean(HeavyVerbRatio, na.rm=T),
            GerundVerbRatio = mean(GerundVerbRatio, na.rm=T),
            WordFrequencyA = mean(WordFrequencyA, na.rm=T),
            WordFrequencyC = mean(WordFrequencyC, na.rm=T),
            NounVerbRatio = mean(NounVerbRatio, na.rm=T),
            ContentAllWordRatio = mean(ContentAllWordRatio, na.rm=T))
Av2$condition <- factor(Av2$condition,c('U', 'C'))  
ggplot(data = Av2, aes(x = condition, y = GerundVerbRatio)) +
  geom_boxplot(
    position = position_dodge(width=0.5),
    outlier.shape = 17,
    outlier.colour = 'red2',
    outlier.size = 0.1,
    aes(fill = condition)) +
  scale_fill_manual(values = c( '#f5f5f5', '#f5f5f5')) + # for boxplot
  #Add the scatter points (treats outliers same as 'inliers')
  geom_jitter(
    position = position_jitter(width = 0.17),
    size = 4.0,
    aes(colour = condition)) +
  scale_color_manual(values = c("#010882", "#02cee0")) +
  theme_classic() +
  coord_cartesian(ylim = c(0.2, 1)) +
  theme(axis.text.x = element_text(size = 22, color = 'black'),axis.title=element_text(size=23)) +
  theme(axis.text.x = element_text(color="black", size=20, angle=0),
        axis.text.y = element_text(color="black", size=16, angle=0))


# Figure 3
mg<-ggplot(dUC,aes(x=SentenceLength,y= WordFrequencyA,color = condition))
mg + scale_color_manual(values=c("#010882", "#02aacc")) +
  theme_classic() +
  geom_jitter(height=0.025,width = 0.07, size = 2.5, shape = 19) +
  geom_smooth(se=FALSE, method = 'gam', size = 1.1) +
  theme(axis.title = element_text(size = 20)) +
  xlab('\nSentence length') +
  ylab('Word frequency\n') +
  theme(axis.text.x = element_text(color="black", 
                                     size=14, angle=0),
          axis.text.y = element_text(color="black", 
                                     size=14, angle=0))

# Figure 4
mg<-ggplot(d1,aes(x=SentenceLength,y= WordFrequencyC,color=Subtype))
mg + scale_color_manual(values=c("#010882", '#0e5e24', '#cf0000', '#fc6d00')) +
  theme_classic() +
  geom_jitter(height=0.025,width = 0.07, size = 2.3) +
  geom_smooth(se=FALSE, method = 'gam', size = 1.1) +
  theme(axis.title = element_text(size = 20)) +
  xlab('\nSentence length') +
  ylab('Word frequency\n')

p<-getViz(g41)
print(plot(p, allTerms = T), pages = 1)