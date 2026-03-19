####################################################
### Natural Resources and Conflict Meta-Analysis ###
### Replication Files                            ###
### William O'Brochta                            ###
### Washington University in St. Louis           ###
####################################################
#sink("logfile", append=TRUE, split=TRUE)

#This replication file requires:
#The file NaturalResourcesConflict.xls to be in this working directory
#The file NaturalResourcesConflictArticleSearch.xlsx provides the full list
#of studies included at all stages of the analysis (SI.5)

require(gdata)
require(lmtest)
require(lfe)
require(lme4)
require(car)
require(rms)
require(robustlmm)
require(xtable)
require(stargazer)
require(metafor)

#Import all data.
data<-as.data.frame(read.xls('NaturalResourcesConflict.xls',sheet=1, header=TRUE))
#Subset out extreme outliers.
data2<-as.data.frame(data[which(data$Precision<1001),])
data3<-as.data.frame(data2[which(data2$Fisher_z<1),])

#SI.3 Table 2 and 3 Moderator Variables summary
data3.1<-data3[,c(4:9,25:54)]
data3_summary<-as.data.frame(matrix(nrow=36, ncol=5))
for(i in 1:36){
  data3_summary[i,1]<-min(data3.1[,i],na.rm=T)
  data3_summary[i,2]<-max(data3.1[,i], na.rm=T)
  data3_summary[i,3]<-sqrt(var(data3.1[,i], na.rm=T))
  data3_summary[i,4]<-mean(data3.1[,i], na.rm=T)
  data3_summary[i,5]<-median(data3.1[,i], na.rm=T)
}

#print(xtable(data3_summary, type = "latex"), file = "filename2.tex")

#SI.3 Table 2 baseline variables
minerals<-data3[data3$Oil==0 & data3$Forests==0,]
99/497
ongoing<-data3[data3$Onset==0 & data3$Incidence==0 & data3$Intensity==0 & data3$Duration==0,]
32/497

#Plot original data.
plot(Precision~Fisher_z, data=data, pch=18)
plot(Precision~Fisher_z, data=data, ylim=c(0,1000), pch=18)

#SI.4.3 Figure 1: Plot subsetted data.
par(mfrow = c(1, 2))
plot(Precision~Fisher_z, data=data3, pch=18, xlab='Fisher z')
abline(v=mean(data3$Fisher_z))
plot(Precision~Fisher_z, data=data3, ylim=c(0,100), pch=18,xlab='Fisher z',ylab='')
abline(v=mean(data3$Fisher_z))

#Influence Plot for all data.
#Remove relevant influence points.
#Refers to ``Influence Analysis" section in main text
model6.2<-lm(Total_t~Precision, data=data)
summary(model6.2)
infl<-influence.measures(model6.2)
which(apply(infl$is.inf,1,any))
summary(infl)
influencePlot(model6.2)


# SI.4.3 Funnel graph statistics
#Determine Any Effect of Full Sample Mean
mean(data[['Fisher_z']])
mean(data[['Precision']])
median(data[['Fisher_z']])
median(data[['Precision']])
#Huge skew introduced by extremely high Fisher z values.
#High precision results do not impact Fisher z estimate.

mean(data[['Total_t']])
mean(data[['Obs']])-mean(data[['Expl']])
1-pt(6.328162,16028)
#P value of 0 indicates t is different from 0.
#Thus, excluded results drive the entire effect.

#Correlation between PCC and Fisher z
summary(lm(data3$PCC_Total~data3$Fisher_z))

#Average effect size and precision.
mean(data3[['Fisher_z']])
mean(data3[['Precision']])
median(data3[['Fisher_z']])
median(data3[['Precision']])

#Mean of 20 highest precision
#Estimated effect is 0.019
data4<-data3[with(data3,order(-Precision)),]
data4<-data4[1:20,]
mean(data4$Fisher_z)

mean(data4$Total_t)
mean(data4[['Obs']])-mean(data4[['Expl']])
1-pt(0.2730391,45020)
#P value of 0.392 indicates t not different from 0.

#Random effects weighted average
funnel1<-rma(yi=Total_t, sei=SE, data=data3[data3$SE!=0 & data3$SE<3,])

#Default is weights=1/(SE^2)

#Table 1 in main text
#Publication Bias, Precision=1/SE
#Test of whether publication selection exists
#Ho:Intercept=0. Rejected. There is publication bias.
#Ho:Precision=0 (PET) Reject. There is a true effect.

#Table 1, Model 1 in main text
model6<-lm(Total_t~Precision, data=data3)
summary(model6)

#SI.4.5 Table 4, Models 1 through 3
#Subset to most common measure for conflict
model6.0.1<-lm(Total_t~Precision, data=data3[data3$Onset==1,])
summary(model6.0.1)

#Most common measure of natural resource wealth
model6.0.1<-lm(Total_t~Precision, data=data3[data3$Oil==1,])
summary(model6.0.1)

#Both combined
model6.0.2<-lm(Total_t~Precision, data=data3[data3$Oil==1 & data3$Onset==1,])
summary(model6.0.2)

model6.0.3<-lm(Total_t~Precision, data=data3[data3$Oil==1 & data3$Onset==1 & data3$The_90s==1,])
summary(model6.0.3)

#Table 1, model 2 in main text
#PEESE estimate of genuine effect.
model6.1<-lm(Total_t~SE+Precision+0, data=data3)
summary(model6.1)

model6.1.1<-lm(Total_t~SE+Precision+0, data=data3[data3$Oil==1,])
summary(model6.1.1)

#SI.4.5 Table 5, models 1-3
#Still no effect
model6.1.2<-lm(Total_t~SE+Precision+0, data=data3[data3$Oil==1 & data3$Onset==1,])
summary(model6.1.2)

model6.1.3<-lm(Total_t~SE+Precision+0, data=data3[data3$Oil==1 & data3$Onset==1 & data3$The_90s==1,])
summary(model6.1.3)

#Code to construct table 1 in main text
stargazer(model6, model6.1)
#SI.3 Table 4
stargazer(model6.0.1, model6.0.2, model6.0.3)
#SI.3 Table 5
stargazer(model6.1.1, model6.1.2, model6.1.3)


###General to Specific Deletion###
#Shown here are the full models without variables deleted
#as well as the final models from general to specific deletion

#Simple WLS (SE2=1/SE^2)
model7<-lm(Fisher_z~Estimates+Year+Impact+Cites_yr+Reviewed+Obs+Expl+
             Europe+Primary_Commodity_Exports+Discovery+Oil+Forests+
             Internal_Conflict+Intensity+Incidence+Duration+Past_Conflict+
             War_Only+Polity+Ethnic_Fractionalization+GDP+Mountains+Interaction+
             Probit+Fixed_Effects+GIS+The_60s+The_70s+The_80s+The_90s, 
           data=data3, weights=SE2)
summary(model7)

model7.1<-lm(Fisher_z~Reviewed+
               Europe+Primary_Commodity_Exports+Oil+
               Incidence+Duration+
               Polity+Ethnic_Fractionalization+Mountains+
               Fixed_Effects, 
           data=data3, weights=SE2)
summary(model7.1)
bptest(model7.1)


#Author Fixed Effects
model8<-lm(Fisher_z~Author+Estimates+Year+Impact+Cites_yr+Reviewed+Obs+Expl+
             Europe+Primary_Commodity_Exports+Discovery+Oil+Forests+
             Internal_Conflict+Intensity+Incidence+Duration+Past_Conflict+
             War_Only+Polity+Ethnic_Fractionalization+GDP+Mountains+Interaction+
             Probit+Fixed_Effects+GIS+The_60s+The_70s+The_80s+The_90s,
             data=data3, weights=SE2)
summary(model8)

model8.1<-lm(Fisher_z~Author+Reviewed+
             Europe+Primary_Commodity_Exports+Oil+
             Incidence+Duration+
            Polity+Ethnic_Fractionalization+Mountains+
             Fixed_Effects,
           data=data3, weights=SE2)
summary(model8.1)


#Random Effects
qt(0.05,442)
model3<-lmer(Fisher_z~Estimates+Year+Impact+Cites_yr+Reviewed+Obs+Expl+
               Europe+Primary_Commodity_Exports+Discovery+Oil+Forests+
               Internal_Conflict+Intensity+Incidence+Duration+Past_Conflict+
               War_Only+Polity+Ethnic_Fractionalization+GDP+Mountains+Interaction+
               Probit+Fixed_Effects+GIS+The_60s+The_70s+The_80s+The_90s+
               (1|Author), data=data3, weights=SE2)
summary(model3)

model3.1<-lmer(Fisher_z~Reviewed+
                 Europe+Primary_Commodity_Exports+Oil+
                 Incidence+Duration+
                 Polity+Ethnic_Fractionalization+Mountains+
                 Fixed_Effects+
               (1|Author), data=data3, weights=SE2)
summary(model3.1)

#Fixed or Random Effects Hausman test for GLM objects
phtest_glmer <- function (glmerMod, glmMod, ...)  {
  coef.wi <- coef(glmMod)
  coef.re <- fixef(glmerMod)
  vcov.wi <- vcov(glmMod)
  vcov.re <- vcov(glmerMod)
  names.wi <- names(coef.wi)
  names.re <- names(coef.re)
  coef.h <- names.re[names.re %in% names.wi]
  dbeta <- coef.wi[coef.h] - coef.re[coef.h]
  df <- length(dbeta)
  dvcov <- vcov.re[coef.h, coef.h] - vcov.wi[coef.h, coef.h]
  stat <- abs(t(dbeta) %*% as.matrix(solve(dvcov)) %*% dbeta)
  pval <- pchisq(stat, df = df, lower.tail = FALSE)
  names(stat) <- "chisq"
  parameter <- df
  names(parameter) <- "df"
  alternative <- "one model is inconsistent"
  res <- list(statistic = stat, p.value = pval, parameter = parameter, 
              method = "Hausman Test",  alternative = alternative,
              data.name=deparse(getCall(glmerMod)$data))
  class(res) <- "htest"
  return(res)
}

phtest_glmer(model3.1,model8.1)
#p value=1, so no difference.

#Cluster Robust SE
model9<-ols(Fisher_z~Estimates+Year+Impact+Cites_yr+Reviewed+Obs+Expl+
              Europe+Primary_Commodity_Exports+Discovery+Oil+Forests+
              Internal_Conflict+Intensity+Incidence+Duration+Past_Conflict+
              War_Only+Polity+Ethnic_Fractionalization+GDP+Mountains+Interaction+
              Probit+Fixed_Effects+GIS+The_60s+The_70s+The_80s+The_90s,
              data=data3, weights=SE2, x=T,y=T)
robcov(model9, cluster=data3$Author)
bptest(model9)

model9.1<-ols(Fisher_z~Reviewed+
                Europe+Primary_Commodity_Exports+Oil+
                Incidence+Duration+
                Polity+Ethnic_Fractionalization+Mountains+
                Fixed_Effects,
            data=data3, weights=SE2, x=T,y=T)
robcov(model9.1, cluster=data3$Author)


model9.2<-ols(Fisher_z~Reviewed+
                Europe+Primary_Commodity_Exports+Oil+
                Incidence+Duration+
                Polity+Ethnic_Fractionalization+Mountains+
                Fixed_Effects+Author,
              data=data3, weights=SE2, x=T,y=T)
robcov(model9.2, cluster=data3$Author)


#SI.4.6.1 Table 6: General to Specific Results
stargazer(model8.1, model3.1, model7.1, robcov(model9.2, cluster=data3$Author), single.row=F)
#SI.4.8 Table 8: Full models before general to specific approach 
stargazer(model7, model3, model8, robcov(model9, cluster=data3$Author), single.row=TRUE)


###Investigate Effects of Specific Variables.###
#SI.4.6.2 Figure 2: Reviewed Moderator Boxplot
par(mfrow = c(1, 1))
boxplot(Fisher_z~Reviewed, data=data3, names=c('Not Reviewed','Reviewed'), ylab='Fisher z')

#The following are individual regressions used to construct
#SI.4.6.2 Table 7, column 2
#Primary_Commodity_Exports
#Significant in all specifications.
sum(data3$Primary_Commodity_Exports)
model5.0<-lm(Fisher_z~Primary_Commodity_Exports, data=data3, weights=SE2)
summary(model5.0)
model5.1.0<-lm(Fisher_z~Author+Primary_Commodity_Exports, data=data3, weights=SE2)
summary(model5.1.0)
model5.1.1<-lmer(Fisher_z~Primary_Commodity_Exports+
                 (1|Author), data=data3, weights=SE2)
summary(model5.1.1)
phtest_glmer(model5.1.1,model5.1.0)

model5.2.1<-ols(Fisher_z~Primary_Commodity_Exports+Author
                , data=data3, weights=SE2, x=T,y=T)
robcov(model5.2.1, cluster=data3$Author)



#Oil
#Negative here vs. positive in specifications.
nrow(data3)-sum(data3$Oil)
model5.3.0<-lm(Fisher_z~Oil, data=data3, weights=SE2)
summary(model5.3.0)
model5.4.0<-lm(Fisher_z~Author+Oil, data=data3, weights=SE2)
summary(model5.4.0)
model5.4.1<-lmer(Fisher_z~Oil+
                   (1|Author), data=data3, weights=SE2)
summary(model5.4.1)
phtest_glmer(model5.4.1,model5.4.0)

model5.5.1<-ols(Fisher_z~Oil+Author
                , data=data3, weights=SE2, x=T,y=T)
robcov(model5.5.1, cluster=data3$Author)


#Ethnic_Fractionalization
#Negative in two of these specifications.
nrow(data3)-sum(data3$Ethnic_Fractionalization)
model5.6.0<-lm(Fisher_z~Ethnic_Fractionalization, data=data3, weights=SE2)
summary(model5.6.0)
model5.7.0<-lm(Fisher_z~Author+Ethnic_Fractionalization, data=data3, weights=SE2)
summary(model5.7.0)
model5.7.1<-lmer(Fisher_z~Ethnic_Fractionalization+
                   (1|Author), data=data3, weights=SE2)
summary(model5.7.1)
phtest_glmer(model5.7.1,model5.7.0)

model5.8.1<-ols(Fisher_z~Ethnic_Fractionalization+Author
                , data=data3, weights=SE2, x=T,y=T)
robcov(model5.8.1, cluster=data3$Author)


#Mountains
#Significant in all specifications.
nrow(data3)-sum(data3$Mountains)
model5.12.0<-lm(Fisher_z~Mountains, data=data3, weights=SE2)
summary(model5.12.0)
model5.13.0<-lm(Fisher_z~Author+Mountains, data=data3, weights=SE2)
summary(model5.13.0)
model5.13.1<-lmer(Fisher_z~Mountains+
                   (1|Author), data=data3, weights=SE2)
summary(model5.13.1)
phtest_glmer(model5.13.1,model5.13.0)

model5.14.1<-ols(Fisher_z~Mountains+Author
                 , data=data3, weights=SE2, x=T,y=T)
robcov(model5.14.1, cluster=data3$Author)

#Reviewed
#Significant and positive
nrow(data3)-sum(data3$Reviewed)
model5.15.0<-lm(Fisher_z~Reviewed, data=data3, weights=SE2)
summary(model5.15.0)
model5.16.0<-lm(Fisher_z~Author+Reviewed, data=data3, weights=SE2)
summary(model5.16.0)
model5.16.1<-lmer(Fisher_z~Reviewed+
                   (1|Author), data=data3, weights=SE2)
summary(model5.16.1)
phtest_glmer(model5.16.1,model5.16.0)

model5.17.1<-ols(Fisher_z~Reviewed+Author
                 , data=data3, weights=SE2, x=T,y=T)
robcov(model5.17.1, cluster=data3$Author)

#Incidence
sum(data3$Incidence)
model5.18.0<-lm(Fisher_z~Incidence, data=data3, weights=SE2)
summary(model5.18.0)
model5.19.0<-lm(Fisher_z~Author+Incidence, data=data3, weights=SE2)
summary(model5.19.0)
model5.19.1<-lmer(Fisher_z~Incidence+
                   (1|Author), data=data3, weights=SE2)
summary(model5.19.1)
phtest_glmer(model5.19.1,model5.19.0)

model5.20.1<-ols(Fisher_z~Incidence+Author
                 , data=data3, weights=SE2, x=T,y=T)
robcov(model5.20.1, cluster=data3$Author)

#Duration
#Only 24 samples. Not significant alone.
sum(data3$Duration)
model5.21.0<-lm(Fisher_z~Duration, data=data3, weights=SE2)
summary(model5.21.0)
model5.22.0<-lm(Fisher_z~Author+Duration, data=data3, weights=SE2)
summary(model5.22.0)
model5.22.1<-lmer(Fisher_z~Duration+
                   (1|Author), data=data3, weights=SE2)
summary(model5.22.1)
phtest_glmer(model5.22.1,model5.22.0)

model5.23.1<-ols(Fisher_z~Duration+Author
                 , data=data3, weights=SE2, x=T,y=T)
robcov(model5.23.1, cluster=data3$Author)

#Fixed Effects
#Only 43 samples. Not stable sign.
sum(data3$Fixed_Effects)
model5.24.0<-lm(Fisher_z~Fixed_Effects, data=data3, weights=SE2)
summary(model5.24.0)
model5.25.0<-lm(Fisher_z~Author+Fixed_Effects, data=data3, weights=SE2)
summary(model5.25.0)
model5.25.1<-lmer(Fisher_z~Fixed_Effects+
                   (1|Author), data=data3, weights=SE2)
summary(model5.25.1)
phtest_glmer(model5.25.1,model5.25.0)

model5.26.1<-ols(Fisher_z~Fixed_Effects+Author
                 , data=data3, weights=SE2, x=T,y=T)
robcov(model5.26.1, cluster=data3$Author)


#Polity
#291 samples
sum(data3$Polity)
model5.27.0<-lm(Fisher_z~Polity, data=data3, weights=SE2)
summary(model5.27.0)
model5.28.0<-lm(Fisher_z~Author+Polity, data=data3, weights=SE2)
summary(model5.28.0)
model5.28.1<-lmer(Fisher_z~Polity+
                    (1|Author), data=data3, weights=SE2)
summary(model5.28.1)
phtest_glmer(model5.28.1,model5.28.0)

model5.29.1<-ols(Fisher_z~Polity+Author
                 , data=data3, weights=SE2, x=T,y=T)
robcov(model5.29.1, cluster=data3$Author)


#Europe
#384 samples
sum(data3$Europe)
model5.30.0<-lm(Fisher_z~Europe, data=data3, weights=SE2)
summary(model5.30.0)
model5.31.0<-lm(Fisher_z~Author+Europe, data=data3, weights=SE2)
summary(model5.31.0)
model5.31.1<-lmer(Fisher_z~Europe+
                    (1|Author), data=data3, weights=SE2)
summary(model5.31.1)
phtest_glmer(model5.31.1,model5.31.0)

model5.32.1<-ols(Fisher_z~Europe+Author
                 , data=data3, weights=SE2, x=T,y=T)
robcov(model5.32.1, cluster=data3$Author)

#sink()



