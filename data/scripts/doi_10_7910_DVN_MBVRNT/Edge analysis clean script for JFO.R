library(Hmisc)
library(lme4)
library(ggplot2)
library(lattice)
library(glmmADMB)
library(influence.ME)
library(AICcmodavg)
library(MuMIn)
library(reshape)
library(car)
library(epiR)
library(hier.part)
library(insight)
library(performance)
library(psych)
library(glmmTMB)
library(glmmADMB)
library(lmerTest)
library(VGAM)

library(scales) 

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}



setwd('/Users/Mike 1/Desktop/BA Meta analysis/Savannah Work/Savannah data')

source('/Users/Mike 1/Desktop/QSG/biostats.R')

edge<-read.csv('edgeeffects31223 for JFO.csv',header=TRUE)

str(edge)
table(edge$species)  
table(edge$species, edge$specialist)  
str(edge)
tapply( edge$numberof.nests, edge$study,sum)

e1<-edge

#As a percentage
e1$standardizednestsuccess<-e1$standardizednestsuccess*100

e1$edgemidpoint
e1$edgemidpoint2  #going with this one

table(e1$species)
length(e1$species)

hist(e1 $edgemidpoint)
hist(e1 $edgemidpoint, breaks=30)  

hist(e1 $edgemidpoint2)
hist(e1 $edgemidpoint2, breaks=30)

hist(e1$standardizednestsuccess, breaks=30) 
hist(e1$standardizedDNS, breaks=30)  #data bet. 0-1....  This data is more left skewed.
qplot(e1$standardizednestsuccess, e1$standardizedDNS) 
cor(e1$standardizednestsuccess, e1$standardizedDNS)  

table(e1$agcoverbuffer) #most studies have low ag cover...


p<-qplot(     edgemidpoint2, standardizednestsuccess, data=subset(e1,specialist=="forest"))
p<-qplot(     edgemidpoint2, standardizednestsuccess, facets=.~specialist, data=subset(e1,specialist=="shrubland"))

p +geom_smooth(method='glm')

p<-qplot(     edgemidpoint2, standardizednestsuccess, data=subset(e1,specialist=="forest"))
p +geom_smooth(method='loess',span=.95, color=I("black"))  +    
    labs(
        x = "Distance to forest edge (m)", 
        y = "        Mature forest species 
        standardized nest survival (%)", 
        title = ""  
      ) +  theme_classic()
  
  
#Trying revised grouping: nesttypeCutoff1m
names(e1)
p<-qplot(     edgemidpoint2, standardizednestsuccess, facets=~specialist, data=e1, color = nesttypeCutoff1m)
p +geom_smooth(method='glm')

p<-qplot(     edgemidpoint2, standardizednestsuccess, data=e1, color = specialist)
p +geom_smooth(method='glm')



##############################################################################################################################
##############################################################################################################################



FB<-subset(e1,specialist=="forest")
FB<-subset(e1,specialist=="shrubland")

names(FB)


dim(FB)

table(FB$study)

hist(FB$standardizednestsuccess) 

range(FB$weight)

#FB$edgemidpoint2 <-scale(FB$edgemidpoint2)
FB$edgemidpoint2

#Used to look at model significance/ CIs, but not for model selection
#Default (REML=T)
fitnull<-lmer(standardizednestsuccess ~ 1 + (1|species)  + (1|study) ,data=FB, weights= weight)  
fit1<-lmer(standardizednestsuccess ~ edgemidpoint2 + (1|species)  + (1|study) ,data=FB, weights= weight)  
#models with nest type
fit12<-lmer(standardizednestsuccess ~ edgemidpoint2 + nesttypeCutoff1m + (1|species)  + (1|study) ,data=FB, weights= weight) #NS
fit13<-lmer(standardizednestsuccess ~ edgemidpoint2 * nesttypeCutoff1m + (1|species)  + (1|study) ,data=FB, weights= weight) #NS



#REML=F - for AIC.
fitnull<-lmer(standardizednestsuccess ~ 1 + (1|species)  + (1|study) ,data=FB, weights= weight, REML=F)  
fit1<-lmer(standardizednestsuccess ~ edgemidpoint2 + (1|species)  + (1|study) ,data=FB, weights= weight, REML=F)  
#models with nest type
fit12<-lmer(standardizednestsuccess ~ edgemidpoint2 + nesttypeCutoff1m + (1|species)  + (1|study) ,data=FB, weights= weight, REML=F) #NS
fit13<-lmer(standardizednestsuccess ~ edgemidpoint2 * nesttypeCutoff1m + (1|species)  + (1|study) ,data=FB, weights= weight, REML=F) #NS

modlist<-list(fitnull, fit1, fit12, fit13)
summary(model.avg(modlist))


fit1<-lmer(standardizedDNS ~ edgemidpoint2 + (1|species)  + (1|study) ,data=FB, weights= weight)  
summary(fit1) #still sig. for forest birds, and NS for shrubland birds



confint(fit1, method="Wald")

confint(fit12, method="Wald")  



#without weights?
fit1woW<-lmer(standardizednestsuccess ~ edgemidpoint2  + (1|species)  + (1|study) ,data=FB, , REML=F) #Still significant

#compare w/ AIC?
fit1<-lmer(standardizednestsuccess ~ edgemidpoint2 + (1|species)  + (1|study) ,data=FB, weights= weight, REML=F)  

modlist<-list(fit1woW, fit1)
summary(model.avg(modlist))

AIC(fit1woW)
AIC(fit1) #pretty much the same for forest, or shrubland, birds w/ or w/o weights





#R squared values
r.squaredGLMM(fit1)

vars <- insight::get_variance(fit1)
(r2_marginal <- vars$var.fixed / (vars$var.fixed + vars$var.random + vars$var.residual))
(r2_conditional <- (vars$var.fixed + vars$var.random) / (vars$var.fixed + vars$var.random + vars$var.residual))

r2_nakagawa(fit1)

icc(fit1)
(icc_adjusted <- vars$var.random / ( vars$var.random + vars$var.residual))
(icc_conditional<- vars$var.random / (vars$var.fixed +vars$var.random + vars$var.residual))



r.squaredGLMM(fitnull)

vars <- insight::get_variance(fitnull)
(r2_marginal <- vars$var.fixed / (vars$var.fixed + vars$var.random + vars$var.residual))
(r2_conditional <- (vars$var.fixed + vars$var.random) / (vars$var.fixed + vars$var.random + vars$var.residual))

r2_nakagawa(fitnull)

icc(fitnull)
(icc_adjusted <- vars$var.random / ( vars$var.random + vars$var.residual))
(icc_conditional<- vars$var.random / (vars$var.fixed +vars$var.random + vars$var.residual))





#looking at variation in random effects. Note on nest survival (no interactions/random slopes)
plot4<-ranef(fit1,condVar=TRUE)
dotplot(plot4)[['study']][1]
dotplot(plot4)[['species']][1]

plot4<-ranef(fitnull,condVar=TRUE)
dotplot(plot4)[['study']][1]
dotplot(plot4)[['species']][1]


outliers_list <- check_outliers(fit1)
if (require("see")) {
  plot(outliers_list)}
insight::get_data(fit1)[outliers_list, ]


outliers_list <- check_outliers(fit1, method='cook')
if (require("see")) {
  plot(outliers_list)}
insight::get_data(fit1)[outliers_list, ]

check_model(fit1)


############################################################


table(e1$species)
FB<-subset(e1,species=="WOTH")   
FB<-subset(e1,species=="OVEN") 
FB<-subset(e1,species=="HETH") 
FB<-subset(e1,species=="PRAW")
FB<-subset(e1,species=="RBGR")  
summary(FB)

dim(FB)

hist(FB$standardizednestsuccess) 

range(FB$weight)

FB$edgemidpoint2 <-scale(FB$edgemidpoint2)
FB$edgemidpoint2
#mean of 0, SD =1

fit1<-lmer(standardizednestsuccess ~ edgemidpoint2 + (1|study) ,data=FB, weights= weight)  
summary(fit1)


#check assumptions, w/o RE
fit1<-lm(standardizednestsuccess ~ edgemidpoint2  ,data=FB, weights= weight)  

plot(fit1)

confint(fit1, method="Wald")  


r.squaredGLMM(fit1)

vars <- insight::get_variance(fit1)
(r2_marginal <- vars$var.fixed / (vars$var.fixed + vars$var.random + vars$var.residual))
(r2_conditional <- (vars$var.fixed + vars$var.random) / (vars$var.fixed + vars$var.random + vars$var.residual))

r2_nakagawa(fit1)

icc(fit1)
(icc_adjusted <- vars$var.random / ( vars$var.random + vars$var.residual))
(icc_conditional<- vars$var.random / (vars$var.fixed +vars$var.random + vars$var.residual))




#without weights?
fit1woW<-lmer(standardizednestsuccess ~ edgemidpoint2   + (1|study) ,data=FB, , REML=F) 
#compare w/ AIC?
fit1<-lmer(standardizednestsuccess ~ edgemidpoint2  + (1|study) ,data=FB, weights= weight, REML=F)  

modlist<-list(fit1woW, fit1)
summary(model.avg(modlist))

AIC(fit1woW)
AIC(fit1) 





######################

library(ggeffects)

m4<-fit1

mydf <- ggpredict(m4, terms="edgemidpoint2[all]")

p<-plot(mydf)


#forest birds
p1<-p+ geom_point(data = FB, aes(x=edgemidpoint2, y=standardizednestsuccess, size= weight) , show.legend = F  ) +   scale_size_continuous(range = c(.5,2),  breaks =c(0, .35, .7)) +  
    labs(
      x = "Distance to forest edge (m)", 
      y = "   Mature forest species 
      standardized nest survival (%)", 
      title = ""  
    )+  theme_classic()
    
    ########
    
    p1
      
  
  # Plot the fitted straight line using geom_line
  mydf <- ggpredict(m4, terms=c("edgemidpoint2"))


# Create a new dataset for the weighted smooth line
new_data <- data.frame(
  x = mydf$x,
  y = mydf$predicted,
  ymin = mydf$conf.low,
  ymax = mydf$conf.high
)

  
plot <- ggplot(FB, aes(x=edgemidpoint2, y=standardizednestsuccess)   ) +
  geom_point(size= FB$weight, show.legend = F) +  # scatter plot of the data points
  scale_size_continuous(range = c(.5,3),  breaks =c(0, .35, .7)) +  labs(
      x = "Distance to forest edge (m)", 
      y = "   Mature forest species 
      standardized nest survival (%)", 
      title = ""  
    )+  theme_classic()
 plot +
  geom_line(data = mydf, aes(x = x, y = predicted), color = "blue", size=1.1) +  # fitted line
  geom_ribbon(data = new_data, aes(x = x, y=y,ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.2) +
  geom_smooth(method='loess',span=.95, color=I("red"), fill='red', alpha = 0.2, size=1.1) 
##########




#HETH
fit1<-lmer(standardizednestsuccess ~ edgemidpoint2 + (1|study) ,data=FB, weights= weight)  

mydf <- ggpredict(fit1, terms="edgemidpoint2[all]")

p<-plot(mydf)

p2<-p+ geom_point(data = FB, aes(x=edgemidpoint2, y=standardizednestsuccess, size= weight) , show.legend = F ) +   scale_size_continuous(range = c(.5,2),  breaks =c(0, .35, .7)) +  
  
  labs(
    
    x = "Distance to forest edge (m)", 
    
    y = "Hermit Thrush 
      standardized nest survival (%)", 
    
    title = ""  
    
  ) 
p2+ coord_cartesian(ylim=c(3.6,90), xlim=c(15,750)) +  theme_classic()









#######
#Shrubland bird plot 
library(ggeffects)
fit1<-lmer(standardizednestsuccess ~ edgemidpoint2 + (1|species)  + (1|study) ,data=FB, weights= weight)  


mydf <- ggpredict(fit1, terms="edgemidpoint2[all]")

p<-plot(mydf)

p1<-p+ geom_point(data = FB, aes(x=edgemidpoint2, y=standardizednestsuccess, size= weight) , show.legend = F  ) +   scale_size_continuous(range = c(.5,2),  breaks =c(0, .35, .7)) +  
    labs(
      x = "Distance to edge (m)", 
      y = "   Shrubland species 
      standardized nest survival (%)", 
      title = ""  
    )+  theme_classic()
    p1
    ########





