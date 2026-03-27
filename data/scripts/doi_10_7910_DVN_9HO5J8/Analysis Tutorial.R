# Data analysis for cultural multilevel selection research: a tutorial
# Tim Waring, 2017, timothy.waring@maine.edu
# University of Maine


# Description
# This R script is an analytical supplement to an article which describes how and why to use a cultural multilevel selection investigation to study the emergence of sustainability-related behaviors and cultural traits. The theory presented in the article, and the analysis we demonstrate below are not limited to environmental behavior, and may be applied to any appropriate behavioral dataset.  

# Here we provide a practical, reusable, and open-source analysis example for researchers interested in using cultural multilevel selection methods to analyize their data. If you use or build on the code, please cite:  
	
# Designing cultural multilevel selection research for sustainability science. **Kline, Waring & Salerno**. (2018) *Sustainability Science*  
	
# Here we demonstrate two simple methods which will help researchers gain intuition about the statistical features of their datasets and whether group-level cultural selection may be at play.  Because this code generates random data, running the code multiple times will produce different datasets, and different results.

# This script has three sections.
# A - Generating a group-structured dataset
# B - Using Rogers' Inequality
# C - Using the Price Equation





#### A - Generating a group-structured dataset ####
groups <- 8
indivs <- 20

# Generate a table of group-wise parameters which will be used to create the actual dataset. These include the presence of the trait under study, and its costs benefits.

means <- data.frame(group=(1:groups),
										trait_freq=round(runif(groups),2),
										group_cost=sample(10,groups,replace=T),
										group_benefit=sample(10,groups,replace=T),
										group_sd=round(runif(groups, min=1, max=3),2))

means


# Build a frame for our simulated dataset,
group<-data.frame(group=(1:groups))
indiv<-data.frame(indiv=(1:indivs))
data<-merge(indiv,group)[,c(2,1)];rm(indiv,group)
data<-merge(data,means)

#add individual level variation,
data$trait<-rbinom(indivs*groups,size=1,prob=data$trait_freq)
data$cost<-round(rnorm(indivs*groups,mean=data$group_cost,sd=data$group_sd),2)
data$benefit<-round(rnorm(indivs*groups,mean=data$group_benefit,sd=data$group_sd),2)

# compute a simple random individual measure of fitness,
data$net <- data$benefit - data$cost
data<-data[,c("group","indiv","trait","net")]

# finish simulated dataframe:
head(data)










#### B - Using Rogers' Inequality ####

# Step 1: Estimate *b*, fitness benefit of altruism across whole population
# To do this we calculate the average fitnesses of altruists and non-altruists and substract them.

# mean fitness of altruists
altruist_fitness <- mean(data$net[data$trait == 1 ])

# mean fitness of non-altruists
nonaltruist_fitness <- mean(data$net[data$trait == 0 ])

# global average fitness benefit of altruism
b <- altruist_fitness - nonaltruist_fitness

b




# Step 2: Estimate *c*, average fitness cost of altruism within groups
#First, make a group-wise data summary.

library(dplyr)
gstats<-as.data.frame(
	data[,c("group","trait","net")] %>%
		group_by(group) %>%
		summarise_all(mean))

gstats

# Next, we calculate the fitness cost of altruism *within each group*.

# First, compute nean fitness in groups by trait (altruist, non-altruist).
fit_gt <- data %>%
	group_by(group,trait) %>%
	summarize(fit = mean(net))

# Put mean groupwise fitness by trait into columns.
fit_a <- fit_gt[fit_gt$trait == 1 ,c(1,3)]
fit_n <- fit_gt[fit_gt$trait == 0 ,c(1,3)]

# Merge altruist fitness with groupwise dataframe.
gstats<-left_join(gstats, fit_a, by="group")
gstats$fit_a <- gstats$fit
gstats$fit <- NULL

# Merge non-altruist fitness with groupwise dataframe.
gstats<-left_join(gstats, fit_n, by="group")
gstats$fit_n <- gstats$fit
gstats$fit <- NULL

# Clean up
rm(fit_a, fit_n, fit_gt)

# Calculate fitness cost of altruism in each group.
gstats$cost <- gstats$fit_n - gstats$fit_a

# And, take the mean across groups. In some cases there may be no altruists or no non-altruists in measured groups. In such cases, instead of using `NA`, a better assumption might be to set the fitness of the missing type to zero for that group. But for demonstration, `na.rm` will do.

c<-mean(gstats$cost, na.rm=TRUE)
c




# Step 3: Estimate *Fst*, proportion of trait variation due to groups
#For this we use the `hierfstat` package to calculate the components of variance. Make sure to specify `diploid=FALSE` since we are working with aDELETE THIS "a" two mututally exclusive cultural types ('alleles') on a single 'gene.' More than just a simple $F_st$, the `varcomp` function provides information on population structure for any number of population groupings. Here we just extract a single $F_st$ value for a single grouping on a single 'gene'.

library(hierfstat)

# Calculate Fst at group level, haploid system
Fst <- varcomp(data[,c("group","trait")], diploid=FALSE)$F
Fst <- as.numeric(Fst); Fst





# Step 4: Evaluate Roger's Inequality: - *Is altruism favored by selection?*
# Roger's inequality states that "_The altruistic allele is favored by selection when the ratio of between- to within-group variance exceeds the ratio of cost to benefit._"  Thus we can compute a simple binary test:

b # fitness benefit of altruism across whole population
c # average fitness cost of altruism within groups
Fst # proportion of trait variation due to groups

c/b < Fst/(1-Fst)









#### C - Using the Price Equation ####

# The Price Equation can be used to compare the relative effects of individual and group selection. We use McElreath and Boyd’s (2007) formulation of Price equation, as follows (in LaTeX):

# \overline{W} \Delta \overline{Z} = Cov(W_g,z_g) + E(Cov(W_{ig},z_{ig}))

#The left hand side of the equation, $\overline{W} \Delta \overline{Z}$ denotes total change, or "evolution," where $\overline{W}$ is mean fitness, and $\Delta \overline{Z}$ is average change in trait. We will not compute these separately here, and they cannot be computed separately without temporal data.

# On the right hand side we have:
# $W_g$ is the average fitness in group $g$,
# $Z_g$ is the trait frequency in group $g$,
# $W_{ig}$ is the fitness of individual $i$, in group $g$, and
# $W_{ig}$ is the trait of individual $i$, in group $g$, so that
# $Cov(W_g,z_g)$ is the covariance between group trait and group fitness, and
# $E(Cov(W_{ig},z_{ig}))$ is average individual-level trait-fitness covariance.
#
# Calculating and comparing these terms is very straightforward. We will use the same group-structued dataset, `data`,INSERT COMMA HERE which we generated in the beginning. For clarity, some code will be redundant with the Rogers example.






## Step 1: Calculate $Cov(W_g,z_g)$, group-level trait-fitness covariance
# Do groups with the higher frequencies of the trait tend to have higher fitnesses?

library(dplyr)
gstats <-
	data %>%
	select(c("group","trait","net")) %>%
	group_by(group) %>%
	summarise_all(mean)

Group_TFC <- cov(gstats$net, gstats$trait)

Group_TFC





## Step 2: Calculate $E(Cov(W_{ig},z_{ig}))$, mean within-group trait-fitness covariance
# Now we calculate within-group trait-fitness covariances, and take their average. This is the same code as the previous step, with one new line. Note that in this example, groups are the same size.

gstats <-
	data %>%
	select(c("group","trait","net")) %>%
	group_by(group) %>%
	mutate(cov=cov(trait,net)) %>% # collect within-group covariances
	summarise_all(mean)

Mean_Indiv_TFC <- mean(gstats$cov)
Mean_Indiv_TFC





## Step 3: Compare $Cov(W_g,z_g)$ and $E(Cov(W_{ig},z_{ig}))$
# Finally, we can compare the two trait-fitness terms, quantitatively. Here we will use a plot. The sum, `Total`, represents the total evolutionary change $\overline{W} \Delta \overline{Z}$. By comparing it with the individual and group-level trait-fitness covariances, we can see how they relate, and which is larger and therefore drives evolution overall.

Total <- Group_TFC + Mean_Indiv_TFC
plotdf <- data.frame(
	Level=c("Total", "Group", "Individual"),
	selection=c(Total,Group_TFC,Mean_Indiv_TFC))

# Comparing the magnitude and sign of the different components of selection is the key portion of this exercise. This can be done by simply inspecting the estimates directly:
plotdf



# And it can also be visually represented. A basic barplot helps to show how individual and group-level selection interact to create total change.

library(ggplot2)
ggplot(data=plotdf, aes(x=Level, y=selection)) +
	ylab("Strength of Selection\n (trait-fitness covariance)") +
	xlab("Level") +
	geom_bar(stat="identity") + scale_x_discrete(limits = c("Total", "Individual", "Group"))

# Two final points should be reiterated. These data do not include change over time (multiple measurements of trait values), but these computations provide the basic approach which could be extended to time series data. Also, because the dataset is randomly generated it may not produce interesting interactions between individual and group-level selection. We suggest running it a few times to see the various possibilities.



#####

# Tim Waring, 2017
# University of Maine