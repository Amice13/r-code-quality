library(tidyverse); library(lme4); library(lmerTest); library(mgcv); library(mgcViz); library(ggplot2)
#---------------------------------------------
# reading in the data

d <- read.csv('LessIsMore_Scores.csv', stringsAsFactors = TRUE)

# separating constrained and unconstrained data
C<-subset(d,d$Condition=='constrained')
U<-subset(d,d$Condition=='unconstrained')

# separating subtypes
dn<-subset(U,U$Subtype=='nfvPPA')
dl<-subset(U,U$Subtype=='lvPPA')
ds<-subset(U,U$Subtype=='svPPA')
dc<-subset(U,U$Subtype=='control')

#---------------------------------------------
#1.1 analysis of COCA
# this data set cannot be released due to licensing issues

# compare log of word types from COCA
c <- read.csv("lemmas_60k_m2495.csv", stringsAsFactors = TRUE)
t.test(c$log ~ c$FunctionContent, data = c)
t.test(subset(c,c$PoS == 'n')$log, subset(c,c$PoS == 'v')$log)
t.test(subset(c,c$HeavyLight== 'Heavy')$log, subset(c,c$HeavyLight== 'Light')$log)
sd(subset(c,c$PoS == 'v' & c$FunctionContent != 'Function')$log, na.rm = TRUE)

#---------------------------------------------
#section 3.1
dUC <- rbind(C,dc)
dUC <- within(dUC, Condition <- relevel(Condition, ref = 'unconstrained'))
summary(lmer(data = dUC, ContentWordRatio ~ Condition + (1|SubjectID)))
summary(lmer(data = dUC, NounVerbRatio ~ Condition + (1|SubjectID)))
summary(lmer(data = dUC, HeavyVerbRatio ~ Condition + (1|SubjectID)))
summary(lmer(data = dUC, GerundVerbRatio ~ Condition + (1|SubjectID)))
summary(lmer(data = dUC, WordFrequencyA ~ Condition + SentenceLength + (1|SubjectID)))
summary(lmer(data = dUC, WordFrequencyC ~ Condition + SentenceLength + (1|SubjectID)))

#section 3.2
g1 <-gam(WordFrequencyA ~ s(SentenceLength) + s(SubjectID, bs = 're'), Method = 'REML', data = dc)
g2 <-gam(WordFrequencyA ~ s(SentenceLength) + s(FunctionWordRatio) + s(SubjectID, bs = 're'), Method = 'REML', data = dc)
g3 <-gam(WordFrequencyC ~ s(SentenceLength) + s(SubjectID, bs = 're'), Method = 'REML', data = dc)
summary(g3)

# create a new data frame for prediction
x0 <- seq(1, 20, by = 0.1)
x1 <- round((runif(length(x0))*10),0)
newd <- data.frame(SentenceLength= x0, SubjectID= x1)
pred <- predict.gam(g1,newd)
newd[which(pred == max(pred)),]

#section 3.3
U <- within(U, Subtype <- relevel(Subtype, ref = 'nfvPPA'))
summary(lmer(data = U, WordFrequencyA ~ Subtype + SentenceLength + (1|SubjectID)))
summary(lmer(data = U, WordFrequencyA ~ Subtype + SentenceLength + FunctionWordRatio + (1|SubjectID)))
summary(lmer(data = U, WordFrequencyC ~ Subtype + SentenceLength + (1|SubjectID)))
summary(lmer(data = U, NEntropy ~ Subtype + (1|SubjectID)))
summary(lmer(data = U, NEntropy ~ WordFrequencyA + (1|SubjectID)))

#section 3.4
m1 <- gam(WordFrequencyA ~ Subtype + s(SentenceLength, by=Subtype) + s(SubjectID, bs = 're') , data = d, Method = 'REML')
m2 <- gam(WordFrequencyA ~ Subtype + s(SentenceLength) + s(SubjectID, bs = 're') , data = d, Method = 'REML')
a <- anova(m1,m2,test='Chisq')

m3 <- gam(WordFrequencyC ~ Subtype + s(SentenceLength, by=Subtype) + s(SubjectID, bs = 're') , data = d, Method = 'REML')
m4 <- gam(WordFrequencyC ~ Subtype + s(SentenceLength) + s(SubjectID, bs = 're') , data = d, Method = 'REML')
m5 <- gam(WordFrequencyC ~ s(SentenceLength) + s(SubjectID, bs = 're') , data = d, Method = 'REML')
a <- anova(m3,m4,test='Chisq')

# m3 and m4 whether the shape of the curves are the same
# m3 and m5 whether there is a difference in the intercept

#_________________________________________
#figures
#_________________________________________

#Figure 1
a1 <- read.csv('Figure1.csv')
a2<- subset(a1,a1$spok> 162000)
p <- ggplot(a2, aes(x = reorder(word, -spok), y = spok, fill = Lexical))
p <- p + geom_bar(stat="identity") +
  scale_fill_manual(values = c( "#b00404", "#d13f3f", "#fa7070","#808080", "#030303")) +
  theme_classic() +
  theme(axis.text.x = element_text(color="#030303", size=12, angle=90, hjust=1, vjust = 0.5))
p


#Figure2
d2 <- rbind(dc,C)
Av = group_by(d2, SubjectID, Condition) %>%
  summarise(FunctionWordRatio = mean(FunctionWordRatio, na.rm=T),
            HeavyVerbRatio = mean(HeavyVerbRatio, na.rm=T),
            GerundVerbRatio2 = mean(GerundVerbRatio, na.rm=T),
            WordFrequencyA = mean(WordFrequencyA, na.rm=T),
            WordFrequencyC = mean(WordFrequencyC, na.rm=T),
            NounVerbRatio = mean(NounVerbRatio, na.rm=T),
            ContentWordRatio = mean(ContentWordRatio, na.rm=T))
Av$Condition <- factor(Av$Condition , levels=c("unconstrained", "constrained"))

ggplot(data = Av, aes(x = Condition, y = HeavyVerbRatio)) +
  geom_boxplot(
    width = 0.5, 
    outlier.shape = 17,
    outlier.colour = 'red2',
    outlier.size = 0.1,
    aes(fill = Condition)) +
  scale_fill_manual(values = c( '#f5f5f5', '#f5f5f5')) + # for boxplot
  #Add the scatter points (treats outliers same as 'inliers')
  geom_jitter(width = .17,
              size = 2,
              aes(colour = Condition)) +
  scale_color_manual(values = c("#050d99", "#e80061")) +
  theme_classic() +
  coord_cartesian(ylim = c(0, 1)) +
  theme(axis.text.x = element_text(size = 10, color = 'black'),axis.title=element_text(size=10)) +
  theme(axis.text.x = element_text(color="black", size=10, angle=0),
        axis.text.y = element_text(color="black", size=25, angle=0))


# Figure 3
df <- subset(dc,dc$SentenceLength<21)
mg<-ggplot(df,aes(x=SentenceLength,y= WordFrequencyA))
mg + theme_classic() +
  geom_jitter(height=0.025,width = 0.07, size = 1.7, color = "#050d99") +
  geom_smooth(se=FALSE, method = 'gam', size = 1, color = "#3b3b3b") +
  theme(axis.title = element_text(size = 20)) +
  xlab('\nSentence length') +
  ylab('Word frequency\n') +
  theme(axis.text.x = element_text(color="black", size=20, angle=0),
        axis.text.y = element_text(color="black", size= 20, angle=0))

plot(g1, select = 1, shade = TRUE, shade.col = "#edeeff", residuals = TRUE, pch = 1, cex = .4)

# Figure 4A
d$Subtype <- factor(d$Subtype, levels=c("control", "nfvPPA", "lvPPA", "svPPA"))
mg<-ggplot(subset(d,d$SentenceLength<21),aes(x=SentenceLength,y= WordFrequencyA,color=Subtype))
mg + scale_color_manual(values=c("#050d99", "#c239fc", "#C90202", "#FC8A05")) +
  theme_classic() +
  geom_jitter(height=0.025,width = 0.07, size = 1.5) +
  geom_smooth(se=FALSE, method = 'gam', size = 1.8) +
  theme(axis.text.x = element_text(color="black", size=20, angle=0),
        axis.text.y = element_text(color="black", size= 20, angle=0))


# Figure 4B
d$Subtype <- factor(d$Subtype, levels=c("control", "nfvPPA", "lvPPA", "svPPA"))
mg<-ggplot(subset(d,d$SentenceLength<21),aes(x=SentenceLength, y= WordFrequencyC, color=Subtype))
mg + scale_color_manual(values=c("#050d99", "#AE39FC", "#C90202", "#FC8A05")) +
  theme_classic() +
  geom_jitter(height=0.025,width = 0.07, size = 1.5) +
  geom_smooth(se=FALSE, method = 'gam', size = 1.8) +
  theme(axis.text.x = element_text(color="black", size=20, angle=0),
        axis.text.y = element_text(color="black", size= 20, angle=0))

