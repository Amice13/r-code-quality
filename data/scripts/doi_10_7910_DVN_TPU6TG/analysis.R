####################################################################################################################################
####################################################################################################################################
####This code was used for Socio-spatial stratification of housing tenure trajectories in Sweden - A longitudinal cohort study######
########################################By Ida Borg, Juta Kawalerowicz and Eva K. Andersson ########################################
####################################################################################################################################
####Note that we cannot publish the micro-data used for the analysis because it belongs to the SCB, Statistics Sweden###############
####################################################################################################################################
####################################################################################################################################

#Load packages
library(readstata13)
library(foreign)
library(TraMineR)
library(cluster)
library(WeightedCluster)
library(dplyr)

## Data

#Our sample is based on those who were 18-25 in 1995 and whose family status changes from a child in a family to any another category between 1994 and 1995. For this we use variables fodelsear and famstf.
#Among those we select sequences with complete record of housing types in the 21 years from 1995. We drop sequences which have at least one missing value for tenure between 1995 and 2016.
#Let us see the number of rows, names and summary statistics for variables in the data Next, we print the share of tenure types in 1995 and 2015.

#Load data
rm(list=ls())
setwd("\\\\micro.intra/XXX/Projects/Tenure types")

#This data consists of individual level tenure data for our sample over the period of 20 years from 1995

data<-read.dta13("/Data/tenureseq20.dta", convert.factors=TRUE, generate.factors=TRUE)

## Data

#Our sample is based on those who were 18-25 in 1995 and whose family status changes from a child in a family to any another category between 1994 and 1995. For this we use variables fodelsear and famstf.
#Among those we select sequences with complete record of housing types in the 21 years from 1995. We drop sequences which have at least one missing value for tenure between 1995 and 2016.
#Let us see the number of rows, names and summary statistics for variables in the data Next, we print the share of tenure types in 1995 and 2015.


#Number of observations
nrow(data)
names(data)
summary(data)

# 1 "Home owner" 2 "Tenant owner 3 "Public rent" 4 "Private rent" 5 "Other"

count(x=data, var=tenure1995, wt=Weight) %>%
  mutate(freq=n/sum(n))

count(x=data, var=tenure2015, wt=Weight) %>%
  mutate(freq=n/sum(n))


### Create sequences

data_lab <- c("Home owner", "Tenant owner", "Public rent", "Private rent", "Other")
data_alphabet <- c(1, 2, 3, 4, 5)
data_states <- c("HO", "BRF", "PuR", "PrR", "Oth")

data_seq <- 
  seqdef(data=data, var=2:22, alphabet=data_alphabet, states=data_states, labels=data_lab, 
         missing=FALSE, weights=data$Weight)

#### Descriptive statistics of sequences

# Print first 10 sequences, all sequences in data set sorted by states from the start and 10 most frequent sequences

par(mfrow=c(2,2))
seqlegend(data_seq, cex = 5.5)
seqiplot(data_seq, with.legend=FALSE, border=NA)
seqIplot(data_seq, sortv="from.start", with.legend=FALSE, border=NA)
seqfplot(data_seq, with.legend=FALSE, border=NA)

#For all sequences print state distribution, entropy, modal state and mean time in state:

library(colorspace)

col1<-sequential_hcl(5, "Blues")[1:2] 
col2<-sequential_hcl(5, "Greens")[1:2] 
col3<-sequential_hcl(5, "magenta")[2]

cols<-c(col1, col2, col3)
swatchplot(cols)

colspace2<-cols


entropy <- seqstatd(data_seq)$Entropy
par(mfrow=c(2,2))
seqdplot(data_seq, with.legend=FALSE, xtlab=c(1995:2015), border=NA,main="a) states distribution and entropy", ylab="Freq./entropy index", xlab="Year", yaxis=TRUE, cpal=colspace2)
lines(entropy, col = "black", lwd = 2, lty = 2)
seqmtplot(data_seq, with.legend=FALSE, xtlab=c(1995:2015), border=NA,main="b) mean time", ylab="Mean time", yaxis=TRUE, cpal=colspace2)
seqmsplot(data_seq, with.legend=FALSE, xtlab=c(1995:2015), border=NA, xlab="Year", ylab="State freq.", yaxis=TRUE, cpal=colspace2, main="c) modal state sequence")
seqlegend(data_seq, cex=2.5, position = "center", cpal=colspace2)


# Tab 10 most common sequences. One striking thing is that housing types are quite stable. Among the most frequent sequences with changing states, we see sequences with fast transition to home ownership. 

seqtab(data_seq)

#### Transition rates
#Next, we show probabilities of transitioning between states. Note that once in a given stated, the probability to remain in this state in the next time period is high, especially for HO (home ownership). Highest transition rates are observed between Oth -> PrR, BRF -> HO and PrR -> HO. Oth is the most unstable state with the ovarall probability (1-0.69) to leave the state. 

data_trate <- seqtrate(data_seq, weighted=TRUE)
print(round(data_trate, 2) )

data_trate.tv <- seqtrate(data_seq, weighted=TRUE, time.varying = T)
#We can also look at the probabilities of transitions between states in a given year. This is for 1995:
print(round(data_trate.tv[,,1], 2) )
#For 2015, we see that the housing states have become more rigid and the probability of transitioning from a given state is lower at the end of the period. 
print(round(data_trate.tv[,,20], 2) )

## Method

#The study employs sequence analysis to find the typical housing trajectories of residents who were in 1995 were aged 18-25 and who moved out of the parental home between 1994 and 1995. We then use multinomial regression analysis to identify how different factors affect the probability of following a given type of housing trajectory. Sequence analysis is a methods concerned with analyzing data sets which comprise of sequences of discrete states. When using this method, we first create a matrix of pairwise dissimilarity that describes to what extend each sequence resembles all other sequences in the data set. There is a number of algorithms for creating dissimilarity matrix, but optimal matching, which we follow in this study, remains one of the most popular options. Next, in order to describe typical sequences observed in data we apply clustering algorithms on the dissimilarity matrix to partition sequences into "ideal types".  

### Optimal matching

#For creating the dissimilarity matrix we use optimal matching (OM), a method that calculates the least costly way to transform one sequence into another. To achieve this the algorithm uses two types of operations: 

#* insertions and deletions (indel) and
#* substitutions

#The costs of insertion and deletion as well are substitution cost are left for the researcher to specify, based on the theoretical considerations sucj as considering whether transitions from some states to others are more costly. Here, we use default option which sets insertion and deletion cost to 1 and a constant value for substitution matrix, where all substitution cost are set to 2. This means that we do not distinguish between more or less costly transitions although we may suspect that, for example, transitioning from private rental to home ownership as more costly than transitioning from tenant ownership to home ownership (perhaps we can look more into this in later reiteration of the paper). Optimal matching was carried out in R using `TraMinerR` package. 

data_om <- seqdist(seqdata=data_seq, method="OM", indel=1, sm="CONSTANT", with.missing=FALSE)

### Typology of trajectories

#For clustering we used a procedure described in `WeightedCluster`:  partition around the medoids (PAM) combined with Ward's hierarchical clustering algorithms. 

#We evaluated average silhouette width (ASW), a statistic used for measuring the quality of partitioning, and considered whether the clusters can be thought of as theoretically interesting. 

#### Sequence clustering

#Cluster analysis aims to create grouping of a set of objects in such as way that the groups are as homogeneous as possible and as different from one another as possible. We use hierarchical agglomerative clustering with Ward's algorithm with Partition around the Medoids (PAM). 

#With hierarchical cluster analysis each object is assigned to its own cluster to start with and then with each step two clusters which are most similar are joined. At each step distances between clusters are recalculated and the procedure continues until there is just one cluster left. The result of this agglomeration can be presented in the form of a tree which is called dendrogram. What determins the number of cluster is a decision at which level to cut the dendrogram tree. 

data_ward <- hclust(as.dist(data_om), method="ward.D2", members=data$Weight)
plot(data_ward)

wardTree<-as.seqtree(data_ward, seqdata=data_seq, diss=data_om, ncluster=8)

#One problem with the use of hierarchical cluster algorithms is that the merging of two groups is done by maximizing a local criterion (Studer 2013). But as noted by Studer, it can often happen that a good choice at local level can lead to medicore choice at a higher level of grouping. We therefore also look at the results from partitioning around medoids (PAM) algorith which has the advantage of maximizing the global criterion and not only a local criterion.

#We combine hierarchical clustering with PAM - we initialize PAM with the solution obtained with hierarchical clustering as the initial choice of the medoids. PAM solutions tend to have better quality of clustering than hierarchical clustering. One issue with PAM is that we need to predefine the number of clusters, but here we are going to compute PAM solutions for a varying number of clusters, between 4 and 15 and then we pick the solution which has theoretical significance and acceptable quality of partitioning statistics.  

#4-15 clusters using herarchical clustering as initial value
data_PAMward <- wcKMedRange(diss=data_om, kvals=4:15, initialclust=data_ward, weights=data$Weight, method="PAM")
plot(data_PAMward)

#### Quality of partitioning

#First, note that if we are guided only by quality of partitioning statistics, we end up with clusters which represent continuous states, among which the largest is a cluster of continuous home ownership. Because we are interested in housing trajectories which represent change, we are going to pick a solution with the highest number of clusters which represent change, while ensuring that the quality of partitioning statistics do not drop too radically in comparison to k-1. The measure which we pay particular attention is ASW, which evaluate coherence of the assignment into clusters. The values for these measures range from -1 to 1, a low value means that groups are not clearly separated. Studer (2013) say that acceptable values for ASW are those above 0.25 (values between 0.25-0.5 are described as weak structure and lower value are described as having no structure). However, for social science applications acceptable ASW values may be even lower.
#Cluster quality and plot for the 4 cluster solution:

wcClusterQuality(data_om, data_PAMward$clustering$cluster4, weights = data$Weight)
seqdplot(data_seq, data_PAMward$clustering$cluster4, border=NA)

#Cluster quality and plot for the 5 cluster solution:

wcClusterQuality(data_om, data_PAMward$clustering$cluster5, weights = data$Weight)
seqdplot(data_seq, data_PAMward$clustering$cluster5, border=NA)

#Cluster quality and plot for the 6 cluster solution:

wcClusterQuality(data_om, data_PAMward$clustering$cluster6, weights = data$Weight)
seqdplot(data_seq, data_PAMward$clustering$cluster6, border=NA)

#Cluster quality and plot for the 7 cluster solution:

wcClusterQuality(data_om, data_PAMward$clustering$cluster7, weights = data$Weight)
seqdplot(data_seq, data_PAMward$clustering$cluster7, border=NA)

#Cluster quality and plot for the 8 cluster solution:

wcClusterQuality(data_om, data_PAMward$clustering$cluster8, weights = data$Weight)
seqdplot(data_seq, data_PAMward$clustering$cluster8, border=NA)

#Cluster quality and plot for the 9 cluster solutions:

wcClusterQuality(data_om, data_PAMward$clustering$cluster9, weights = data$Weight)
seqdplot(data_seq, data_PAMward$clustering$cluster9, border=NA)

#Cluster quality and plot for the 10 cluster solutions:

wcClusterQuality(data_om, data_PAMward$clustering$cluster10, weights = data$Weight)
seqdplot(data_seq, data_PAMward$clustering$cluster10, border=NA)

#Cluster quality and plot for the 11 cluster solutions:

wcClusterQuality(data_om, data_PAMward$clustering$cluster11, weights = data$Weight)
seqdplot(data_seq, data_PAMward$clustering$cluster11, border=NA)

#Cluster quality and plot for the 12 cluster solutions:

wcClusterQuality(data_om, data_PAMward$clustering$cluster12, weights = data$Weight)
seqdplot(data_seq, data_PAMward$clustering$cluster12, border=NA)

#Cluster quality and plot for the 13 cluster solutions:

wcClusterQuality(data_om, data_PAMward$clustering$cluster13, weights = data$Weight)
seqdplot(data_seq, data_PAMward$clustering$cluster13, border=NA)

#Cluster quality and plot for the 14 cluster solutions:

wcClusterQuality(data_om, data_PAMward$clustering$cluster14, weights = data$Weight)
seqdplot(data_seq, data_PAMward$clustering$cluster14, border=NA)

#Cluster quality and plot for the 15 cluster solutions:

wcClusterQuality(data_om, data_PAMward$clustering$cluster15, weights = data$Weight)
seqdplot(data_seq, data_PAMward$clustering$cluster15, border=NA)

#Based on the results above we believe that the solution with 8 clusters represents the best trade-off in terms of theoretical significance and the quality of partitioning. We use as.clustrange function from `WeightedCluster` package to compare different clustering solutions. 

#Measuring the quality of partitions for PAM solutions
wardRange <- as.clustrange(data_PAMward$clustering, diss = data_om, weights = data$Weight, ncluster = 20)
summary(wardRange, max.rank = 2)

#Next, we show clustering quality statistics for different solutions. Note that 4 clusters are a local maximum for ASW (the highest value is for 4 clusters but we don't want it because then we have the clusters of trajectories with no changes). The first plot is adjusted by mean and standard deviation (i.e. standardized results). **ASW** takes values between -1 and 1 and is the average silhouette width (coherence of assignment, high coherence indicates high between group distances and strong within group homogeneity), **HG** goes between -1 and 1 and is Hubert's Gamma (measure of coherence that indicates the capacity of clusters to reproduce the distances and you want it to be high), **PBC** goes between -1 and 1 and is Point Biserial Correlation (it also measures the capacity of the clusters to reproduce distances and you want it to be high), **HC** goes between 0 and 1 and is Hubert's C (it measures the gap between the partitions obtained and the best partition theoretically possible with this number of groups and these distances, you want it to be *low*). 

plot(wardRange, stat=c("ASW", "HG", "PBC", "HC"), norm = "zscore")

#give automatic names
data$cluster8.auto<-seqclustname(data_seq ,wardRange$clustering$cluster8, data_om)
library(RM.weights)
#tabulate clusters
tab.weight(data$cluster8.auto, wt = data$Weight)

#**Representative clusters**

#We can calculate silhouette for each sequence, this is used to pick a sequence which is most characteristic for each group. The most characteristic sequences are at the top of each cluster. 

sil <- wcSilhouetteObs(data_om, data$cluster8.auto, weights=data$Weight, measure = "ASW")
seqIplot(data_seq, data$cluster8.auto, sortv=sil)

#Finally, we show the states distributions in the 8 cluster solution, individual sequences within each cluster and the mean time in each state for 8 clusters:

seqdplot(data_seq, data$cluster8.auto, border = NA)
seqIplot(data_seq, group=data$cluster8.auto, sortv="from.start")
seqmtplot(data_seq, group=data$cluster8.auto)
seqfplot(data_seq, group=data$cluster8.auto, border = NA)

data$trajectory<-""
data$trajectory[data$cluster8.auto=="PuR/8-HO/13"]<-"From public rent to home ownership (N=5,723)"
data$trajectory[data$cluster8.auto=="PuR/4-PrR/2-PuR/15"]<-"Majority public rent (N=4,700)"
data$trajectory[data$cluster8.auto=="PuR/2-PrR/3-BRF/16"]<-"Majority tenant ownership (N=6,633)"
data$trajectory[data$cluster8.auto=="PuR/1-PrR/1-HO/19"]<-"Early entry into home ownership (N=10,476)"
data$trajectory[data$cluster8.auto=="PuR/2-PrR/1-Oth/2-PrR/3-Oth/13"]<-"Majority other rent (N=1,189)"
data$trajectory[data$cluster8.auto=="PrR/1-BRF/8-HO/12"]<-"From tenant ownership to home ownership (N=6,509)"
data$trajectory[data$cluster8.auto=="PrR/8-HO/13"]<-"From private rent to home ownership (N=6,336)"
data$trajectory[data$cluster8.auto=="PuR/1-PrR/3-PuR/2-PrR/15"]<-"Majority private rent (N=4,220)"


data$trajectory<-factor(data$trajectory, levels= c("Early entry into home ownership (N=10,476)",
                                                      "Majority tenant ownership (N=6,633)",
                                                      "From tenant ownership to home ownership (N=6,509)",
                                                      "From private rent to home ownership (N=6,336)",
                                                      "From public rent to home ownership (N=5,723)",
                                                      "Majority public rent (N=4,700)",
                                                      "Majority private rent (N=4,220)",
                                                      "Majority other rent (N=1,189)"))


library(do)
data1<-expand(x=data, weight="Weight")

data1_seq <- 
  seqdef(data=data1, var=2:22, alphabet=data_alphabet, states=data_states, labels=data_lab, 
         missing=FALSE)
  
par(mfrow=c(2,4))
seqdplot(data1_seq, data1$trajectory, with.legend=FALSE, xtlab=c(1995:2015), border=NA, ylab="Freq.", xlab="Year", yaxis=TRUE, cpal=colspace2)
seqfplot(data1_seq, data1$trajectory, with.legend=FALSE, xtlab=c(1995:2015), border=NA, ylab="Freq.", xlab="Year", yaxis=TRUE, cpal=colspace2)

## Multinomial logistic regression

#Below is a code that:

#1. Imports independent variables and merges it with sequences and clusters data 
#2. Selects a subset of variables used for regression
#3. Provides summary statistics for these variables
#4. Encodes, labels and factors relevant variables

clusters<-data[c("seq_id", "cluster8.auto", "trajectory")]

setwd("\\\\micro.intra/projekt/P0903$/P0903_Gem/Juta/Projects/Tenure types")

#load the individual level data
data_wide<-read.dta13("Data/tenure_trajectories_wide.dta", convert.factors=TRUE, generate.factors=TRUE)

#load house price data from 1995
house_prices1995<-read.csv("Data/smahuspriser1995.csv")

#load number of siblings
sibls<-read.dta13("\\\\micro.intra/projekt/P0903$/P0903_Gem/Juta/Projects/Tenure types/Data/sample_parents.dta", convert.factors=TRUE, generate.factors=TRUE)

#load parental education
par_edu<-read.dta13("Data/sample_education.dta", convert.factors=TRUE, generate.factors=TRUE)
#load family type for individuals
fam_typ<-read.dta13("Data/sample_family.dta", convert.factors=TRUE, generate.factors=TRUE)
#load parental income terciles in 1994
par_income_94<-read.dta13("Data/income_fam_1994.dta", convert.factors=TRUE, generate.factors=TRUE)
#load representative tanure types data
tenure_1995<-read.dta13("Data/tenure_1995.dta", convert.factors=TRUE, generate.factors=TRUE)
#load municipality classification
municipality<-read.dta13("Data/Kommungrupper.dta", convert.factors=TRUE, generate.factors=TRUE)

municipality$class_name_en[municipality$class_name_en==""]<-NA
municipality$factor<-factor(municipality$class_name_en)

#merge
data_wide<-data.frame(data_wide, sibls[match(data_wide$p0903_lopnr_personnr, sibls$p0903_lopnr_personnr),])
data_wide<-data.frame(data_wide, par_edu[match(data_wide$p0903_lopnr_personnr, par_edu$p0903_lopnr_personnr),])
data_wide<-data.frame(data_wide, fam_typ[match(data_wide$p0903_lopnr_personnr, fam_typ$p0903_lopnr_personnr),])
data_wide<-data.frame(data_wide, house_prices1995[match(data_wide$kommun, house_prices1995$kammun_code),])
data_wide<-data.frame(data_wide, tenure_1995[match(data_wide$municipality1995, tenure_1995$KOMMUN),])
data_wide<-data.frame(data_wide, clusters[match(data_wide$seq_id, clusters$seq_id),])
data_wide$municipality1995_n<-as.numeric(data_wide$municipality1995)
data_wide<-data.frame(data_wide, municipality[match(data_wide$municipality1995_n, municipality$kommun_code),])
data_wide<-data.frame(data_wide, par_income_94[match(data_wide$p0903_lopnr_personnr, par_income_94$p0903_lopnr_personnr),])

#select only variables of interest
dataset<-data_wide[c("p0903_lopnr_personnr", "kon", "foreign_bckgd", "equiv_tercile", "kommun_birth1", "factor", "trajectory", "tenure_mode_komm", "tenure_overrep_komm", "price_1995",  "sibs_count", "barn_b", "fam_type", "parents_aftergym")]

#select only observations with cluster type
dataset.complete<-dataset[complete.cases(dataset$trajectory),]

#summarize
summary(dataset.complete)

#Encode sex
dataset.complete$sex<-NA
dataset.complete$sex[dataset.complete$kon=="male"]<-1
dataset.complete$sex[dataset.complete$kon=="female"]<-0

#Label variables and values
library(expss)
dataset.complete = apply_labels(dataset.complete,
                                p0903_lopnr_personnr = "Personal ID",
                                sex = "Sex",
                                sex = c("Male" = 1,
                                        "Female" = 0),
                                foreign_bckgd = "Foreign background",
                                foreign_bckgd = c("Foreign" = 1,
                                        "Native" = 0),
                                equiv_tercile    = "Household income in 1994",
                                equiv_tercile    = c("Low" = 1, "Middle" = 2, "High" = 3),
                                kommun_birth1 = "Type of municipality",
                                kommun_birth1 = c("metropolis" = 1, "suburb to metropolis" = 2, "large municipality" = 3,
                                                  "industrial municipality" = 4, "rural or small municipality" = 5, 
                                                  "sparsely populated" = 6),
                                tenure_mode_komm = "Dominant tenure type in 1995",
                                tenure_mode_komm = c("Home-owner" = 1, "Tenant owner" = 2, "Public rent" = 3, "Private rent" = 4),
                                tenure_overrep_komm = "Characteristic tenure type in 1995",
                                tenure_overrep_komm = c("Home-owner" = 1, "Tenant owner" = 2, "Public rent" = 3, "Private rent" = 4),
                                price_1995 = "Small house price in 1995",
                                sibs_count = "Siblings count",
                                barn_b = "Children",
                                barn_b = c("Children"=1, "No children"=0), 
                                fam_type = "Family type",
                                fam_type = c("Married"=1, "Cohabiting"=2, "Single"=3),
                                parents_aftergym="Parental post-secondary education",
                                parents_aftergym=c("Both"=2, "One"=1, "None"=0))

#Make sure factor variables are factors
dataset.complete$sex<-factor(dataset.complete$sex)
dataset.complete$foreign_bckgd<-factor(dataset.complete$foreign_bckgd)
dataset.complete$equiv_tercile<-factor(dataset.complete$equiv_tercile)
dataset.complete$kommun_birth1<-factor(dataset.complete$kommun_birth1)
dataset.complete$tenure_mode_komm<-factor(dataset.complete$tenure_mode_komm)
dataset.complete$tenure_overrep_komm<-factor(dataset.complete$tenure_overrep_komm)
dataset.complete$barn_b<-factor(dataset.complete$barn_b)
dataset.complete$fam_type<-factor(dataset.complete$fam_type)
dataset.complete$parents_aftergym<-factor(dataset.complete$parents_aftergym)

#The dependent variable is the housing trajectory types created in the previous section. 

#The independent variables are described below, summary statistics are in the code above after summary(dataset.complete)

#* **Sex** - whether individual is male or female
#* **Foreign background** - whether at least one parent born outside of Sweden
#* **Parental income tercile in 1995** - based on household income in 1994
#* **Type of municipality** - classification of municipalities where individual was resident in 1995
#* **Characteristic tenure type in 1995** - compared to Swedish tabulation of tenure types for households, which type was most overrepresented in this municipality
#* **Small house price in 1995** - price for small house in 1995, data from Ida
#* **Siblings count** - based on who were the parents in 1994, full siblings counts for 1, half siblings for 0.5
#* **Children** - whether individual was a parent in 1995
#* **Family type** - type of union: married, cohabiting or single
#* **Parental post-secondary education** - if parents in 1994 had education higher than gymnasium

library(nnet)

#Note that the model output has two blocks - one with coefficients #and one with standard errors. The coefficients are displayed in log #odds. 

dataset.complete<-dataset.complete[complete.cases(dataset.complete[c("trajectory","sex","foreign_bckgd", "equiv_tercile", "factor", "tenure_overrep_komm", "price_1995", "sibs_count", "barn_b", "fam_type", "parents_aftergym")]),]

library(pollster)
library(dplyr)

#large city is reference category
factor(dataset.complete$factor)
dataset.complete$factor<-relevel(dataset.complete$factor, "Large cities")

dataset.complete$factor<-factor(dataset.complete$factor, levels=c("Metropolitan areas", "Suburbs to metropolitan areas", "Large cities", "Industrial municipalities", "Other large municipalities","Middle-sized towns", "Other small municipalities", "Rural municipalities", "Sparsely populated municipalities"))

model <- multinom(trajectory ~ sex + foreign_bckgd + equiv_tercile + factor + tenure_overrep_komm + price_1995 + sibs_count + barn_b + fam_type + parents_aftergym, data=dataset.complete)

summary(model)

### Results

library(stargazer, message=FALSE, warning=FALSE, include=FALSE)

stargazer(model, type = "html",  covariate.labels = c("Sex: Male", "Background: Foreign", "Parental income (Ref=Low): Middle", "Parental income (Ref=Low): High", "Municipality (Ref=Large city): Industrial", "Municipality (Ref=Large city): Metropolitan areas", "Municipality (Ref=Large city): Middle-sized towns", "Municipality (Ref=Large city): Other large", "Municipality (Ref=Large city): Other small", "Municipality (Ref=Large city): Rural", "Municipality (Ref=Large city): Sparsely populated", "Municipality (Ref=Large city): Suburbs to metropolitan areas", "Characteristic tenure (Ref=Home-owner): Tenant owner", "Characteristic tenure (Ref=Home-owner): Public rent", "Characteristic tenure (Ref=Home-owner): Private rent", "Small house price in 1995", "Sibling count", "Children: Yes", "Family type (Ref=Married): Cohabiting", "Family type (Ref=Married): Single", "Parental post-sec. education (Ref=None): One", "Parental post-sec. education (Ref=None): Both"), add.lines=list(c("n", nrow(dataset.complete), nrow(dataset.complete), nrow(dataset.complete), nrow(dataset.complete), nrow(dataset.complete), nrow(dataset.complete), nrow(dataset.complete))),out="model.html")
          
#Relative risk ratios
summary(model)

z<-summary(model)$coefficients/summary(model)$standard.errors
p<-(1-pnorm(abs(z), 0, 1))*2

rrr<-exp(coef(model))

rrr<-exp(coef(model))
se_rrr<-exp(coef(model))*summary(model)$standard.errors

stargazer(model, type = "html", se=list(se_rrr), p=list(p), coef=list(rrr),  covariate.labels = c("Sex: Male", "Background: Foreign", "Parental income (Ref=Low): Middle", "Parental income (Ref=Low): High", "Municipality (Ref=Large city): Industrial", "Municipality (Ref=Ref=Large city): Metropolitan areas", "Municipality (Ref=Large city): Middle-sized towns", "Municipality (Ref=Large city): Other large", "Municipality (Ref=Large city): Other small", "Municipality (Ref=Large city): Rural", "Municipality (Ref=Large city): Sparsely populated", "Municipality (Ref=Large city): Suburbs to metropolitan areas", "Characteristic tenure (Ref=Home-owner): Tenant owner", "Characteristic tenure (Ref=Home-owner): Public rent", "Characteristic tenure (Ref=Home-owner): Private rent", "Small house price in 1995", "Sibling count", "Children: Yes", "Family type (Ref=Married): Cohabiting", "Family type (Ref=Married): Single", "Parental post-sec. education (Ref=None): One", "Parental post-sec. education (Ref=None): Both"), add.lines=list(c("n", nrow(dataset.complete), nrow(dataset.complete), nrow(dataset.complete), nrow(dataset.complete), nrow(dataset.complete), nrow(dataset.complete), nrow(dataset.complete))),out="model_rrr.html")

### Vizualisation

levels(dataset.complete$trajectory)
#Create shorter names without N's
dataset.complete$trajectory<-as.factor(gsub("\\s*\\([^\\)]+\\)", "", as.character(dataset.complete$trajectory)))


dataset.complete$trajectory<-factor(as.character(dataset.complete$trajectory), levels=c("Early entry into home ownership","Majority tenant ownership","From tenant ownership to home ownership","From private rent to home ownership","From public rent to home ownership","Majority public rent","Majority private rent","Majority other rent"))

library(effects)

#sex
eff_sex<-predictorEffects(model, ~ sex)
#foreign born
eff_fb<-predictorEffects(model, ~ foreign_bckgd)
#household income tercile
eff_ink<-predictorEffects(model, ~ equiv_tercile)
#municipality type
eff_fac<-predictorEffects(model, ~ factor)
#overrepresented tenure type
eff_t<-predictorEffects(model, ~ tenure_overrep_komm)
#housing price 1995
eff_pri<-predictorEffects(model, ~ price_1995)
#siblings
eff_sib<-predictorEffects(model, ~ sibs_count)
#child
eff_bar<-predictorEffects(model, ~ barn_b)
#family type
eff_f<-predictorEffects(model, ~ fam_type)
#parental education
eff_par<-predictorEffects(model, ~ parents_aftergym)

plot(eff_sex,
     axes=list(grid=TRUE, 
               x=list(rug=FALSE),
               y=list(style="lines")), main="",
     lines=list(col=c("#be2928", "#fe4500", "#f78601", "#eaCD00", "#9eff07", "#00f000", "#01cc91", "#00b0d8")),
     lattice=list(key.args=list(columns=1), strip=list(factor.names=FALSE, cex=0.52), layout=c(4,2)))

plot(eff_fb,
     axes=list(grid=TRUE, 
               x=list(rug=FALSE),
               y=list(style="lines")), main="",
     lines=list(col=c("#be2928", "#fe4500", "#f78601", "#eaCD00", "#9eff07", "#00f000", "#01cc91", "#00b0d8")),
     lattice=list(key.args=list(columns=1), strip=list(factor.names=FALSE, cex=0.52), layout=c(4,2)))

plot(eff_ink,
     axes=list(grid=TRUE, 
               x=list(rug=FALSE),
               y=list(style="lines")), main="",
     lines=list(col=c("#be2928", "#fe4500", "#f78601", "#eaCD00", "#9eff07", "#00f000", "#01cc91", "#00b0d8")),
     lattice=list(key.args=list(columns=1), strip=list(factor.names=FALSE, cex=0.52), layout=c(4,2)))

plot(eff_t,
     axes=list(grid=TRUE, 
               x=list(rug=FALSE),
               y=list(style="lines")), main="",
     lines=list(col=c("#be2928", "#fe4500", "#f78601", "#eaCD00", "#9eff07", "#00f000", "#01cc91", "#00b0d8")),
     lattice=list(key.args=list(columns=1), strip=list(factor.names=FALSE, cex=0.52), layout=c(4,2)))

plot(eff_pri,
     axes=list(grid=TRUE, 
               x=list(rug=FALSE),
               y=list(style="lines")), main="",
     lines=list(col=c("#be2928", "#fe4500", "#f78601", "#eaCD00", "#9eff07", "#00f000", "#01cc91", "#00b0d8")),
     lattice=list(key.args=list(columns=1), strip=list(factor.names=FALSE, cex=0.52), layout=c(4,2)))

plot(eff_sib,
     axes=list(grid=TRUE, 
               x=list(rug=FALSE),
               y=list(style="lines")), main="",
     lines=list(col=c("#be2928", "#fe4500", "#f78601", "#eaCD00", "#9eff07", "#00f000", "#01cc91", "#00b0d8")),
     lattice=list(key.args=list(columns=1), strip=list(factor.names=FALSE, cex=0.52), layout=c(4,2)))

plot(eff_bar,
     axes=list(grid=TRUE, 
               x=list(rug=FALSE),
               y=list(style="lines")), main="",
     lines=list(col=c("#be2928", "#fe4500", "#f78601", "#eaCD00", "#9eff07", "#00f000", "#01cc91", "#00b0d8")),
     lattice=list(key.args=list(columns=1), strip=list(factor.names=FALSE, cex=0.52), layout=c(4,2)))

plot(eff_f,
     axes=list(grid=TRUE, 
               x=list(rug=FALSE),
               y=list(style="lines")), main="",
     lines=list(col=c("#be2928", "#fe4500", "#f78601", "#eaCD00", "#9eff07", "#00f000", "#01cc91", "#00b0d8")),
     lattice=list(key.args=list(columns=1), strip=list(factor.names=FALSE, cex=0.52), layout=c(4,2)))

plot(eff_par,
     axes=list(grid=TRUE, 
               x=list(rug=FALSE),
               y=list(style="lines")), main="",
     lines=list(col=c("#be2928", "#fe4500", "#f78601", "#eaCD00", "#9eff07", "#00f000", "#01cc91", "#00b0d8")),
     lattice=list(key.args=list(columns=1), strip=list(factor.names=FALSE, cex=0.52), layout=c(4,2)))

plot(eff_fac, rug=FALSE,
     axes=list(grid=TRUE, 
               x=list(rotate=45)),
          lines=list(multiline=TRUE, col=c("#be2928", "#fe4500", "#f78601", "#eaCD00", "#9eff07", "#00f000", "#01cc91", "#00b0d8")), main="")





