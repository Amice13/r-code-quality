#########################R-script for ERGM-analysis on co-branding process###################################
######################### Written by Vidar Stevens - stevens@essb.eur.nl#################################################

##################Strategy of analysis##########################
#To perform the ERGM analyses I followed the analysis and model selection strategy that has been proposed by Goodreau (2007)
#For each dimension of the learning variable (i.e. construction, co-construction, and constructive conflict) four, and sometimes five models, will be tested.
#A first model is always the so-called Bernoulli/Erdös-Rényi model, which with only a single term (i.e. edges) captures the density of the network under study (Goodreau, 2007:239).
#Second, I include the selected endogenous terms in the analysis. Within ERGM studies this is considered as a 'reduced homogeneous realization-dependent model' (idem: 240).
#Third, I add the exogenous nodal covariates (i.e. amount of years representative, top-priority Minister, type of organization, personal meaningfulness, and chairman) to the model with the endogenous terms. 
#Fourth, I analyse a model that includes the endogenous terms as well as the exogenous dyadic covariate effects (i.e. belief homophily, trust, and actor importance).
#If necessary, I test a fifth model - a more optimized model of the dyadic covariate effects model - to ensure a better fit of the (final) explanatory model. 
#Hence, for each dimension of the dependent variable I search for four, or sometimes five, 'best fitting models' - and from this sample of models I select the model that is most capable in explaining the learning interaction patters in the empirical data. 

##################Model-selection and goodness of fit criteria######################
#Inspired by the work of Goodreau (2007:238), I use three general approaches to examine the goodness of fit of the models. 
#First, I check for degeneracy and model convergence. A minimum requirement for a model to fit well is for estimation of parameters to converge on finite parameter values. It must also be non-degenerate, that is, not place all of its probability mass on a few networks entirely unlike the observed network, such as a full or empty network.
#Second, I compare the Akaike Information Criterion (AIC) between models. Models that exhibit dyadic independence can be fit with standard logistic regressions, which yield a likelihood measure for the model. Those models that are dyad dependent must be fit with MCMC, which also yields an estimate of the likelihood. I use the given likelihood to calculate AIC in order to compare models; with a lower AIC implying a significant increase in model fit. 
#Third, I view the goodness of fit by plotting the model against higher order statistics. This approach is described in detail by Hunter et al. (2008). The logic entails generating new networks according to the probability distribution implied by the fit model. Because the normalizing constant is still present in the fit models, this must be done using the same MCMC approach employed during the estimation procedure. A statistic of interest is then calculated on the original network and on the set of networks generated from the model, and these are plotted for comparison. If the original network is inconsistent with the networks generated from the model, this suggests that the structure of the network differs from those predicted by the model, and the model is not well fit. Multiple statistics can be compared visually to provide detailed information about the systematic ways in which the data and the model predictions differ.
#For this third approach, the higher order network statistics I specifically use to compare the selected models to include: in-degree, out-degree, edge-wise shared partners, dyad-wise shared partners, and the minimum geodesic distance. Each of these higher-order network statistics are plotted on the log-odds scale for the sake of greater visibility across the covered range of values. 

###################Neccesary packages to use the ERGM method########################
#Install the neccesary packages, load the library, set work directory and read in the csv.file

install.packages("statnet") 
install.packages("ergm")
install.packages("sna")
install.packages("boot")
install.packages("latticeExtra")
library("statnet")
library("sna")
library("boot")
library("ergm")
library("latticeExtra")

###############R-codes per dimension#####################
#Our dependent variable of learning has 3 seperate dimensions: construction, co-construction and constructive conflict.
#These are derived from different scales
#For the analysis of each of these dimensions we will first do the preparations.
#We start with the dimension of construction.

###############construction (preperation)################
#setwd, load table, turn it into adjacency-matrix.
#In the survey we asked each respondent to indicate up to 5 representatives that showed signs of 'construction'learning behaviour to them.
#Hence we transpose the excel matrix, to get a table representating out-going ties.

getwd()
setwd("C:/Users/Vstevens/Box Sync/POSTDOC co-branding")
construction=read.csv(("construction_transponeren.csv"), header=T, row.names=1, sep=';')

#turn the data-files into a matrix-object
m=as.matrix(construction)

#turn the matrix-object into a network adjacency matrix and plot the graph
net=network(m, matrix.type="adjacency", directed=TRUE)
plot(net,displaylabels=T)
summary(net)
network.dyadcount(net)
network.edgecount(net)
network.size(net)
network.density(net)
plot(net,displaylabels=T, mode="circle")

#add datasetTOPK  
datasetTOPK<- read.table("datasetTOPK.csv", header=T, row.names=1, sep=";", stringsAsFactors=FALSE)

#convert attributes of datasetAF to vectors
years_organization <-datasetTOPK[,2]
years_experience <- datasetTOPK[,3]
toppriority_minister <- datasetTOPK[,21]
toppriority_administrativeleader <- datasetTOPK[,22]
controll_minister<-datasetTOPK[,23]
controll_administrativeleader <-datasetTOPK[,24]
experienced_autonomy <-datasetTOPK[,25]
societalvalue <- datasetTOPK[,26]
personalvalue <- datasetTOPK[,27]
chairman1<- datasetTOPK[,28]
projectleader1 <- datasetTOPK[,29]


#attach vectors to the network net
set.vertex.attribute(net,"yearsorganization",years_organization)
set.vertex.attribute(net,"yearsexperience",years_experience)
set.vertex.attribute(net,"toppminister",toppriority_minister)
set.vertex.attribute(net,"toppadml",toppriority_administrativeleader)
set.vertex.attribute(net,"controllminister",controll_minister)
set.vertex.attribute(net,"controlladministrativeleader",controll_administrativeleader)
set.vertex.attribute(net,"societvalue",societalvalue)
set.vertex.attribute(net,"persvalue",personalvalue)
set.vertex.attribute(net,"autonomy",experienced_autonomy)
set.vertex.attribute(net,"chairman",chairman1) #with this vertex attribute, we identify the chairman in the network
set.vertex.attribute(net,"projectleader",projectleader1) 


#add covariates beliefhomophily, trust and earlier billateral contact patterns
beliefhomophily<- as.matrix(read.table("beliefhomo.csv", header=TRUE, row.names=1, sep=";", stringsAsFactors=FALSE))
trust<- as.matrix(read.table("trust.csv", header=TRUE, row.names=1, sep=";", stringsAsFactors=FALSE))
earlierbillateralcontact<- as.matrix(read.table("earlier_billateral_contact.csv", header=TRUE, row.names=1, sep=";", stringsAsFactors=FALSE))
ingenuity<- as.matrix(read.table("ingenuity.csv", header=TRUE, row.names=1, sep=";", stringsAsFactors=FALSE))

diag(beliefhomophily)<- 0
diag(trust)<- 0
diag(earlierbillateralcontact)<- 0
diag(ingenuity)<- 0

###############end of preperation dimension of construction#####################

###############co-construction(preparation)###########################
#In the survey we asked each respondent to indicate up to 5 representatives that showed signs of 'co-construction'learning behaviour to them.
#Hence we transpose the excel matrix, to get a table representating out-going ties.
setwd("C:/Users/Vstevens/Box Sync/POSTDOC co-branding")
coconstruction=read.csv(("coconstruction_transponeren.csv"), header=T, row.names=1, sep=';')

#turn the data-files into a matrix-object
mm=as.matrix(coconstruction)

#turn the matrix-object into a network adjacency matrix and plot the graph
netmm=network(mm, matrix.type="adjacency", directed=TRUE)
plot(netmm,displaylabels=TRUE)
summary(netmm)
network.dyadcount(netmm)
network.edgecount(netmm)
network.size(netmm)
network.density(netmm)
plot(netmm,displaylabels=T, mode="circle")

#add datasetTOPK  
datasetTOPK<- read.table("datasetTOPK.csv", header=T, row.names=1, sep=";", stringsAsFactors=FALSE)

#convert attributes of datasetAF to vectors
years_organization <-datasetTOPK[,2]
years_experience <- datasetTOPK[,3]
toppriority_minister <- datasetTOPK[,21]
toppriority_administrativeleader <- datasetTOPK[,22]
controll_minister<-datasetTOPK[,23]
controll_administrativeleader <-datasetTOPK[,24]
experienced_autonomy <-datasetTOPK[,25]
societalvalue <- datasetTOPK[,26]
personalvalue <- datasetTOPK[,27]
chairman1<- datasetTOPK[,28]
projectleader1 <- datasetTOPK[,29]

#attach vectors to the network netmm
set.vertex.attribute(netmm,"yearsorganization",years_organization)
set.vertex.attribute(netmm,"yearsexperience",years_experience)
set.vertex.attribute(netmm,"toppminister",toppriority_minister)
set.vertex.attribute(netmm,"toppadml",toppriority_administrativeleader)
set.vertex.attribute(netmm,"controllminister",controll_minister)
set.vertex.attribute(netmm,"controlladministrativeleader",controll_administrativeleader)
set.vertex.attribute(netmm,"societvalue",societalvalue)
set.vertex.attribute(netmm,"persvalue",personalvalue)
set.vertex.attribute(netmm,"autonomy",experienced_autonomy)
set.vertex.attribute(netmm,"chairman",chairman1) #with this vertex attribute, we identify the chairman in the network
set.vertex.attribute(netmm,"projectleader",projectleader1) 



#add covariates beliefhomophily, trust and earlier billateral contact patterns
beliefhomophily<- as.matrix(read.table("beliefhomo.csv", header=TRUE, row.names=1, sep=";", stringsAsFactors=FALSE))
trust<- as.matrix(read.table("trust.csv", header=TRUE, row.names=1, sep=";", stringsAsFactors=FALSE))
earlierbillateralcontact<- as.matrix(read.table("earlier_billateral_contact.csv", header=TRUE, row.names=1, sep=";", stringsAsFactors=FALSE))
ingenuity<- as.matrix(read.table("ingenuity.csv", header=TRUE, row.names=1, sep=";", stringsAsFactors=FALSE))

diag(beliefhomophily)<- 0
diag(trust)<- 0
diag(earlierbillateralcontact)<- 0
diag(ingenuity)<- 0

#######################end of preperation dimension of co-construction#################################

############################constructive conflict (preparation)##################################
#In the survey we asked each respondent to indicate up to 5 representatives that showed signs of 'constructive conflict'learning behaviour to them.
#Hence we transpose the excel matrix, to get a table representating out-going ties.
getwd()
setwd("C:/Users/Vstevens/Box Sync/POSTDOC co-branding")
constructiveconflict=read.csv(("constructiveconflict_transponeren.csv"), header=T, row.names=1, sep=';')

#turn the data-files into a matrix-object
mmm=as.matrix(constructiveconflict)

#turn the matrix-object into a network adjacency matrix and plot the graph
netmmm=network(mmm, matrix.type="adjacency", directed=TRUE)
plot(netmmm,displaylabels=TRUE)
summary(netmmm)
network.dyadcount(netmmm)
network.edgecount(netmmm)
network.size(netmmm)
network.density(netmmm)
plot(netmmm,displaylabels=T, mode="circle")

#add datasetTOPK  
datasetTOPK<- read.table("datasetTOPK.csv", header=T, row.names=1, sep=";", stringsAsFactors=FALSE)

#convert attributes of datasetAF to vectors#
years_organization <-datasetTOPK[,2]
years_experience <- datasetTOPK[,3]
toppriority_minister <- datasetTOPK[,21]
toppriority_administrativeleader <- datasetTOPK[,22]
controll_minister<-datasetTOPK[,23]
controll_administrativeleader <-datasetTOPK[,24]
experienced_autonomy <-datasetTOPK[,25]
societalvalue <- datasetTOPK[,26]
personalvalue <- datasetTOPK[,27]
chairman1<- datasetTOPK[,28]
projectleader1 <- datasetTOPK[,29]

#attach vectors to the network netmmm#
set.vertex.attribute(netmmm,"yearsorganization",years_organization)
set.vertex.attribute(netmmm,"yearsexperience",years_experience)
set.vertex.attribute(netmmm,"toppminister",toppriority_minister)
set.vertex.attribute(netmmm,"toppadml",toppriority_administrativeleader)
set.vertex.attribute(netmmm,"controllminister",controll_minister)
set.vertex.attribute(netmmm,"controlladministrativeleader",controll_administrativeleader)
set.vertex.attribute(netmmm,"societvalue",societalvalue)
set.vertex.attribute(netmmm,"persvalue",personalvalue)
set.vertex.attribute(netmmm,"autonomy",experienced_autonomy)
set.vertex.attribute(netmmm,"chairman",chairman1) #with this vertex attribute, we identify the chairman in the network
set.vertex.attribute(netmmm,"projectleader",projectleader1)

#add covariates beliefhomophily, trust and earlier billateral contact patterns
beliefhomophily<- as.matrix(read.table("beliefhomo.csv", header=TRUE, row.names=1, sep=";", stringsAsFactors=FALSE))
trust<- as.matrix(read.table("trust.csv", header=TRUE, row.names=1, sep=";", stringsAsFactors=FALSE))
earlierbillateralcontact<- as.matrix(read.table("earlier_billateral_contact.csv", header=TRUE, row.names=1, sep=";", stringsAsFactors=FALSE))
ingenuity<- as.matrix(read.table("ingenuity.csv", header=TRUE, row.names=1, sep=";", stringsAsFactors=FALSE))

diag(beliefhomophily)<- 0
diag(trust)<- 0
diag(earlierbillateralcontact)<- 0
diag(ingenuity)<- 0

######################## end of preparation dimension of constructive conflict###############################

##############################R-Analysis dimension of construction#################################
c1=ergm(net~edges,
        control=control.ergm(seed=1603, force.main = T))#force-main=T wanneer geen dependence term is included.
summary(c1)
mcmc.diagnostics(c1)#
sink(file="c1.txt") 
print(summary(c1))
c1$coef
print(c1$mle.lik) # also print the log likelihood
print(mcmc.diagnostics(c1)) #also print diagnostics
sink()

#goodness of fit statistics in various file formats.
pdf("c1.pdf")
gof_c1 <- gof(c1 ~ idegree + odegree + espartners + dspartners + distance, verbose = FALSE)
par(mfrow = c(3, 2))
plot(gof_c1)
print(mcmc.diagnostics(c1))
dev.off()

setEPS()
postscript("c1.eps")
par(mfrow = c(3, 2))
plot(gof_c1)
dev.off()
dev.off()

win.metafile("c1.wmf")
par(mfrow = c(3, 2))
plot(gof_c1)
dev.off()
dev.off()

#edges significant means, according to Harris (2014:46), "..in this case the coeffcient for the edges term is negative and signifcant, indicating that the densitiy of the network is below 50%".

####################Second ERGM analysis with the other endogeneous terms (edges, mutual, and gwodegree)##############
c2=ergm(net~edges+gwodegree(0.7)+mutual,
        control=control.ergm(seed=1603, force.main = T))
summary(c2)
##The gwodegree(decay) term also includes the normal gwodegree in the analysis.
mcmc.diagnostics(c2)#
sink(file="c2.txt") 
print(summary(c2))
c2$coef
print(c2$mle.lik) # also print the log likelihood
print(mcmc.diagnostics(c2)) #also print diagnostics
sink()

#goodness of fit statistics in various file formats.
pdf("c2.pdf")
gof_c2 <- gof(c2 ~ idegree + odegree + espartners + dspartners + distance, verbose = FALSE)
par(mfrow = c(3, 2))
plot(gof_c2)
print(mcmc.diagnostics(c2))
dev.off()

setEPS()
postscript("c2.eps")
par(mfrow = c(3, 2))
plot(gof_c2)
dev.off()
dev.off()

win.metafile("c2.wmf")
par(mfrow = c(3, 2))
plot(gof_c2)
dev.off()
dev.off()

############################Third ERGM analysis with nodal covariate effects############################# 

c3=ergm(net~edges+gwodegree(0.7)+ttriple+nodecov("chairman")+nodecov("societvalue"),
        control=control.ergm(seed=1603, force.main = T))
summary(c3)
mcmc.diagnostics(c3)#
sink(file="c3.txt") 
print(summary(c3))
c3$coef
print(c3$mle.lik) # also print the log likelihood
print(mcmc.diagnostics(c3)) #also print diagnostics
sink()

#goodness of fit statistics in various file formats.
pdf("c3.pdf")
gof_c3 <- gof(c3 ~ idegree + odegree + espartners + dspartners + distance, verbose = FALSE)
par(mfrow = c(3, 2))
plot(gof_c3)
print(mcmc.diagnostics(c3))
dev.off()

setEPS()
postscript("c3.eps")
par(mfrow = c(3, 2))
plot(gof_c3)
dev.off()
dev.off()

win.metafile("c3.wmf")
par(mfrow = c(3, 2))
plot(gof_c3)
dev.off()
dev.off()

################Fourth ERGM analysis with dyadic and nodal covariate effects##################

cModel=ergm(net~edges+mutual+gwodegree(0.7)+ttriple+edgecov(earlierbillateralcontact)+edgecov(beliefhomophily)+edgecov(ingenuity)+edgecov(trust)+nodecov("chairman")+nodecov("societvalue"),
           control=control.ergm(seed=1603, force.main = T))

summary(cModel)
mcmc.diagnostics(cModel)#
sink(file="cModel.txt") 
print(summary(cModel))
cModel$coef
print(cModel$mle.lik) # also print the log likelihood
print(mcmc.diagnostics(cModel)) #also print diagnostics
sink()

#goodness of fit statistics in various file formats.
pdf("cModel.pdf")
gof_cModel <- gof(cModel ~ idegree + odegree + espartners + dspartners + distance, verbose = FALSE)
par(mfrow = c(3, 2))
plot(gof_cModel)
print(mcmc.diagnostics(cModel))
dev.off()

setEPS()
postscript("cModel.eps")
par(mfrow = c(3, 2))
plot(gof_cModel)
dev.off()
dev.off()

win.metafile("cModel.wmf")
par(mfrow = c(3, 2))
plot(gof_cModel)
dev.off()
dev.off()


cModel1=ergm(net~edges+mutual+gwodegree(0.7)+ttriple+edgecov(earlierbillateralcontact)+nodecov("chairman")+nodecov("societvalue"),
            control=control.ergm(seed=1603, force.main = T))
summary(cMode1l)
mcmc.diagnostics(cModel1)#
sink(file="cModel1.txt") 
print(summary(cModel1))
cModel1$coef
print(cModel1$mle.lik) # also print the log likelihood
print(mcmc.diagnostics(cModel1)) #also print diagnostics
sink()

#goodness of fit statistics in various file formats.
pdf("cModel1.pdf")
gof_cModel1 <- gof(cModel1 ~ idegree + odegree + espartners + dspartners + distance, verbose = FALSE)
par(mfrow = c(3, 2))
plot(gof_cModel1)
print(mcmc.diagnostics(cModel1))
dev.off()

setEPS()
postscript("cModel1.eps")
par(mfrow = c(3, 2))
plot(gof_cModel1)
dev.off()
dev.off()

win.metafile("cModel1.wmf")
par(mfrow = c(3, 2))
plot(gof_cModel1)
dev.off()
dev.off()


###############################################################################################################


cc1=ergm(netmm~edges,
        control=control.ergm(seed=1603, force.main = T))#force-main=T wanneer geen dependence term is included.
summary(cc1)
mcmc.diagnostics(cc1)#
sink(file="cc1.txt") 
print(summary(cc1))
cc1$coef
print(cc1$mle.lik) # also print the log likelihood
print(mcmc.diagnostics(cc1)) #also print diagnostics
sink()

#goodness of fit statistics in various file formats.
pdf("cc1.pdf")
gof_cc1 <- gof(cc1 ~ idegree + odegree + espartners + dspartners + distance, verbose = FALSE)
par(mfrow = c(3, 2))
plot(gof_cc1)
print(mcmc.diagnostics(cc1))
dev.off()

setEPS()
postscript("cc1.eps")
par(mfrow = c(3, 2))
plot(gof_cc1)
dev.off()
dev.off()

win.metafile("cc1.wmf")
par(mfrow = c(3, 2))
plot(gof_cc1)
dev.off()
dev.off()

#edges significant means, according to Harris (2014:46), "..in this case the coeffcient for the edges term is negative and signifcant, indicating that the densitiy of the network is below 50%".

####################Second ERGM analysis with the other endogeneous terms (edges, mutual, and gwodegree)##############
cc2=ergm(net~edges+mutual,
        control=control.ergm(seed=1603, force.main = T))
summary(cc2)
##The gwodegree(decay) term also includes the normal gwodegree in the analysis.
mcmc.diagnostics(cc2)#
sink(file="cc2.txt") 
print(summary(cc2))
cc2$coef
print(cc2$mle.lik) # also print the log likelihood
print(mcmc.diagnostics(cc2)) #also print diagnostics
sink()

#goodness of fit statistics in various file formats.
pdf("cc2.pdf")
gof_cc2 <- gof(cc2 ~ idegree + odegree + espartners + dspartners + distance, verbose = FALSE)
par(mfrow = c(3, 2))
plot(gof_cc2)
print(mcmc.diagnostics(cc2))
dev.off()

setEPS()
postscript("cc2.eps")
par(mfrow = c(3, 2))
plot(gof_cc2)
dev.off()
dev.off()

win.metafile("cc2.wmf")
par(mfrow = c(3, 2))
plot(gof_cc2)
dev.off()
dev.off()

############################Third ERGM analysis with nodal covariate effects############################# 

cc3=ergm(net~edges+gwodegree(0.7)+ttriple+nodecov("chairman")+nodecov("societvalue"),
        control=control.ergm(seed=1603, force.main = T))
summary(cc3)
mcmc.diagnostics(cc3)#
sink(file="cc3.txt") 
print(summary(cc3))
cc3$coef
print(cc3$mle.lik) # also print the log likelihood
print(mcmc.diagnostics(cc3)) #also print diagnostics
sink()

#goodness of fit statistics in various file formats.
pdf("cc3.pdf")
gof_cc3 <- gof(cc3 ~ idegree + odegree + espartners + dspartners + distance, verbose = FALSE)
par(mfrow = c(3, 2))
plot(gof_cc3)
print(mcmc.diagnostics(cc3))
dev.off()

setEPS()
postscript("cc3.eps")
par(mfrow = c(3, 2))
plot(gof_cc3)
dev.off()
dev.off()

win.metafile("cc3.wmf")
par(mfrow = c(3, 2))
plot(gof_cc3)
dev.off()
dev.off()



ccModel=ergm(netmm~edges+mutual+gwodegree(0.7)+ttriple+edgecov(beliefhomophily)+edgecov(earlierbillateralcontact)+edgecov(ingenuity)+edgecov(trust)+nodecov("chairman")+nodecov("societvalue"),
            control=control.ergm(seed=1603, force.main = T))

summary(ccModel)
mcmc.diagnostics(ccModel)#
sink(file="ccModel.txt") 
print(summary(ccModel))
ccModel$coef
print(ccModel$mle.lik) # also print the log likelihood
print(mcmc.diagnostics(ccModel)) #also print diagnostics
sink()

#goodness of fit statistics in various file formats.
pdf("ccModel.pdf")
gof_ccModel <- gof(ccModel ~ idegree + odegree + espartners + dspartners + distance, verbose = FALSE)
par(mfrow = c(3, 2))
plot(gof_ccModel)
print(mcmc.diagnostics(ccModel))
dev.off()

setEPS()
postscript("ccModel.eps")
par(mfrow = c(3, 2))
plot(gof_ccModel)
dev.off()
dev.off()

win.metafile("ccModel.wmf")
par(mfrow = c(3, 2))
plot(gof_ccModel)
dev.off()
dev.off()

ccModel1=ergm(netmm~edges+mutual+ttriple+edgecov(earlierbillateralcontact)+edgecov(ingenuity)+edgecov(trust)+nodecov("chairman")+nodecov("societvalue"),
             control=control.ergm(seed=1603, force.main = T))

summary(ccModel1)
mcmc.diagnostics(ccModel1)#
sink(file="ccModel1.txt") 
print(summary(ccModel1))
ccModel1$coef
print(ccModel1$mle.lik) # also print the log likelihood
print(mcmc.diagnostics(ccModel1)) #also print diagnostics
sink()

#goodness of fit statistics in various file formats.
pdf("ccModel1.pdf")
gof_ccModel1 <- gof(ccModel1 ~ idegree + odegree + espartners + dspartners + distance, verbose = FALSE)
par(mfrow = c(3, 2))
plot(gof_ccModel1)
print(mcmc.diagnostics(ccModel1))
dev.off()

setEPS()
postscript("ccModel1.eps")
par(mfrow = c(3, 2))
plot(gof_ccModel1)
dev.off()
dev.off()

win.metafile("ccModel1.wmf")
par(mfrow = c(3, 2))
plot(gof_ccModel1)
dev.off()
dev.off()

##############################################################################################################




ccf1=ergm(netmmm~edges,control=control.ergm(seed=1603, force.main = T))

summary(ccf1)
mcmc.diagnostics(ccf1)#
sink(file="ccf1.txt") 
print(summary(ccf1))
ccf1$coef
print(ccf1$mle.lik) # also print the log likelihood
print(mcmc.diagnostics(ccf1)) #also print diagnostics
sink()

#goodness of fit statistics in various file formats.
pdf("ccf1.pdf")
gof_ccf1 <- gof(ccModel1 ~ idegree + odegree + espartners + dspartners + distance, verbose = FALSE)
par(mfrow = c(3, 2))
plot(gof_ccf1)
print(mcmc.diagnostics(ccf1))
dev.off()

setEPS()
postscript("ccf1.eps")
par(mfrow = c(3, 2))
plot(gof_ccf1)
dev.off()
dev.off()

win.metafile("ccf1.wmf")
par(mfrow = c(3, 2))
plot(gof_ccf1)
dev.off()
dev.off()

ccf2=ergm(netmmm~edges+mutual+gwodegree(0.7)+ttriple,control=control.ergm(seed=1603, force.main = T))
summary(ccf2)
mcmc.diagnostics(ccf2)#
sink(file="ccf2.txt") 
print(summary(ccf2))
ccf2$coef
print(ccf2$mle.lik) # also print the log likelihood
print(mcmc.diagnostics(ccf2)) #also print diagnostics
sink()

#goodness of fit statistics in various file formats.
pdf("ccf2.pdf")
gof_ccf2 <- gof(ccf2 ~ idegree + odegree + espartners + dspartners + distance, verbose = FALSE)
par(mfrow = c(3, 2))
plot(gof_ccf2)
print(mcmc.diagnostics(ccf2))
dev.off()

setEPS()
postscript("ccf2.eps")
par(mfrow = c(3, 2))
plot(gof_ccf2)
dev.off()
dev.off()

win.metafile("ccf2.wmf")
par(mfrow = c(3, 2))
plot(gof_ccf2)
dev.off()
dev.off()

ccf3=ergm(netmmm~edges+mutual+gwodegree(0.7)+ttriple+nodecov("chairman")+nodecov("societvalue"),control=control.ergm(seed=1603, force.main = T))
summary(ccf3)
mcmc.diagnostics(ccf3)#
sink(file="ccf3.txt") 
print(summary(ccf3))
ccf3$coef
print(ccf3$mle.lik) # also print the log likelihood
print(mcmc.diagnostics(ccf3)) #also print diagnostics
sink()

#goodness of fit statistics in various file formats.
pdf("ccf3.pdf")
gof_ccf3 <- gof(ccf3 ~ idegree + odegree + espartners + dspartners + distance, verbose = FALSE)
par(mfrow = c(3, 2))
plot(gof_ccf3)
print(mcmc.diagnostics(ccf3))
dev.off()

setEPS()
postscript("ccf3.eps")
par(mfrow = c(3, 2))
plot(gof_ccf3)
dev.off()
dev.off()

win.metafile("ccf3.wmf")
par(mfrow = c(3, 2))
plot(gof_ccf3)
dev.off()
dev.off()

ccf4=ergm(netmmm~edges+mutual+gwodegree(0.7)+ttriple+edgecov(beliefhomophily)+edgecov(earlierbillateralcontact)+edgecov(ingenuity)+edgecov(trust)+nodecov("chairman")+nodecov("societvalue"),
             control=control.ergm(seed=1603, force.main = T))

summary(ccf4)
mcmc.diagnostics(ccf4)#
sink(file="ccf4.txt") 
print(summary(ccf4))
ccf4$coef
print(ccf4$mle.lik) # also print the log likelihood
print(mcmc.diagnostics(ccf4)) #also print diagnostics
sink()

#goodness of fit statistics in various file formats.
pdf("ccf4.pdf")
gof_ccf4 <- gof(ccf4 ~ idegree + odegree + espartners + dspartners + distance, verbose = FALSE)
par(mfrow = c(3, 2))
plot(gof_ccf4)
print(mcmc.diagnostics(ccf4))
dev.off()

setEPS()
postscript("ccf4.eps")
par(mfrow = c(3, 2))
plot(gof_ccf4)
dev.off()
dev.off()

win.metafile("ccf4.wmf")
par(mfrow = c(3, 2))
plot(gof_ccf4)
dev.off()
dev.off()


ccf5=ergm(netmmm~edges+mutual+gwodegree(0.7)+ttriple+edgecov(earlierbillateralcontact)+edgecov(ingenuity)+edgecov(trust)+nodecov("chairman"),
          control=control.ergm(seed=1603, force.main = T))

summary(ccf5)
mcmc.diagnostics(ccf5)#
sink(file="ccf5.txt") 
print(summary(ccf5))
ccf5$coef
print(ccf5$mle.lik) # also print the log likelihood
print(mcmc.diagnostics(ccf5)) #also print diagnostics
sink()

#goodness of fit statistics in various file formats.
pdf("ccf5.pdf")
gof_ccf5 <- gof(ccf5 ~ idegree + odegree + espartners + dspartners + distance, verbose = FALSE)
par(mfrow = c(3, 2))
plot(gof_ccf5)
print(mcmc.diagnostics(ccf5))
dev.off()

setEPS()
postscript("ccf5.eps")
par(mfrow = c(3, 2))
plot(gof_ccf5)
dev.off()
dev.off()

win.metafile("ccf5.wmf")
par(mfrow = c(3, 2))
plot(gof_ccf5)
dev.off()
dev.off()



