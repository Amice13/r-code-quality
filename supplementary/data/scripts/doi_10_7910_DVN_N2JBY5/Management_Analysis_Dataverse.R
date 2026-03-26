#install.packages(c('ggplot2','vegan','betapart','BiodiversityR','dplyr','ggrepel','car','lme4','rstatix')) #Remove hash to install packages necessary for analysis. I have included the hash to make this a comment and not consistently re-download packages as I run this analysis.

library(ggplot2);library(vegan);library(betapart);library(BiodiversityR);library(dplyr);library(ggrepel);library(car);library(lme4);library(rstatix) #This line of code calls the packages up.

std.error <- function(x, na.rm=FALSE) {if(na.rm==TRUE) x <- na.omit(x) 
sd(x)/sqrt(length(x))
} #Defining standard error to calculate it.

path_out = 'C:/Users/brooksh/Dropbox (Smithsonian)/SeaGrant_Retrospective_2022/Dataverse_Upload' #This is defining the path to save various small dataframes to as .csv files. Change this to your preferred folder configuration.

####### 
#### Read in historical species data.
#######

hist <- read.csv("C:/Users/brooksh/Dropbox (Smithsonian)/SeaGrant_Retrospective_2022/Dataverse_Upload/Historical_Covers.csv", header=TRUE, na.strings = "NA") #Reading in historical species data from my computer.
hist[is.na(hist)] <- 0 #Filling empty cells with 0 for analysis purposes.
summary(hist) #Taking a look at the data structure.
hist.nmds <- hist[-c(1:4)] #Removing first columns to make NMDS matrix.
hist.env <- hist[-c(5:15)] #Removing last columns to make environmental matrix.

####### 
#### Do NMDS to historical species data. By and large this is an exploratory look at the historical presence-absence data from McCormick and Somes (1982).
#######

hist.m1 <- metaMDS(hist.nmds, trymax = 100, trace = FALSE) #Doing NMDS to historical species data. Note that there is insufficient data leading to a stress of nearly zero.
hist.m1 #Checking the dimensions of the NMDS and the stress value.
stressplot(hist.m1) #Loking at stressplot of the NMDS to check how it looks. Big hockey puck looking stressplot. This is exploratory to look at the data.
NMDS.hist = data.frame(MDS1 = hist.m1$points[,1], MDS2 = hist.m1$points[,2]) #Making the NMDS dataset for graphing using ggplot2.
ggplot(data = NMDS.hist, aes(MDS1, MDS2)) + geom_point(size = 2) + scale_fill_brewer(palette = "Dark2") + theme(axis.title = element_text(size = 15, vjust = -4)) + theme(text = element_text(size = 12.5)) + theme(panel.background = element_blank()) + theme(axis.line.x.bottom = element_line()) + theme(axis.line.x.top = element_line()) + guides(x.sec="axis") +  theme(axis.text.x.top = element_blank(), axis.ticks.x.top = element_blank()) + theme(axis.line.y.left = element_line()) + theme(axis.line.y.right = element_line()) + guides(y.sec="axis") +  theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) + labs(color = "Treatment") + theme(legend.key=element_rect(fill="white")) #Graphing the NMDS plot of historical species data.

####### 
#### Read in modern species data.
#######

mod <- read.csv("C:/Users/brooksh/Dropbox (Smithsonian)/SeaGrant_Retrospective_2022/Dataverse_Upload/Modern_Covers.csv", header=TRUE, na.strings = "NA") #Reading in historical species data from my computer.
mod.nmds <- mod[-c(1:4)] #Removing first columns to make NMDS matrix.
mod.nmds[is.na(mod.nmds)] <- 0 #Assigning value of 0 to missing data for cover class.
mod.nmds[mod.nmds == '1'] <- 2.5 #Assigning midpoint values of Braun-Blanquet cover classes.
mod.nmds[mod.nmds == '2'] <- 15 #Assigning midpoint values of Braun-Blanquet cover classes.
mod.nmds[mod.nmds == '3'] <- 37.5 #Assigning midpoint values of Braun-Blanquet cover classes.
mod.nmds[mod.nmds == '4'] <- 62.5 #Assigning midpoint values of Braun-Blanquet cover classes.
mod.nmds[mod.nmds == '5'] <- 87.5 #Assigning midpoint values of Braun-Blanquet cover classes.
summary(mod.nmds) #Looking at summary of data from NMDS matrix.
mod.env <- mod[-c(5:37)] #Removing last columns to make environmental matrix
mod.env$Phrag_Cover[is.na(mod.env$Phrag_Cover)] <- 0 #Assigning value of 0 to missing data for cover class.
mod.env$Phrag_Cover[mod.env$Phrag_Cover == '1'] <- 2.5 #Assigning midpoint values of Braun-Blanquet cover classes.
mod.env$Phrag_Cover[mod.env$Phrag_Cover == '2'] <- 15 #Assigning midpoint values of Braun-Blanquet cover classes.
mod.env$Phrag_Cover[mod.env$Phrag_Cover == '3'] <- 37.5 #Assigning midpoint values of Braun-Blanquet cover classes.
mod.env$Phrag_Cover[mod.env$Phrag_Cover == '4'] <- 62.5 #Assigning midpoint values of Braun-Blanquet cover classes.
mod.env$Phrag_Cover[mod.env$Phrag_Cover == '5'] <- 87.5 #Assigning midpoint values of Braun-Blanquet cover classes.
summary(mod.env) #Looking at summary of data from environmental matrix.

####### 
#### Do NMDS to modern species data
#######

mod.m1 <- metaMDS(mod.nmds, trymax = 100, trace = FALSE) #Doing NMDS to modern species data.
mod.m1 #Checking the dimensions of the NMDS and the stress value. Note that the stress value is less than 0.2. Stress values do change each time the NMDS model is run.
stressplot(mod.m1)  #Loking at stressplot of the NMDS to check how it looks.
NMDS.mod = data.frame(NMDS1 = mod.m1$points[,1], NMDS2 = mod.m1$points[,2],group=mod.env$Treatment) #Making the NMDS dataset for graphing using ggplot2.
colnames(NMDS.mod)[3] ="Treatment" #Changing the column name "group" to Treatment for graphing.
ggplot(data = NMDS.mod, aes(NMDS1, NMDS2)) + geom_point(aes(color = Treatment), size = 3) + scale_fill_brewer(palette = "Dark2") + theme(axis.title = element_text(size = 15, vjust = -4)) + theme(text = element_text(size = 12.5)) + theme(panel.background = element_blank()) + theme(axis.line.x.bottom = element_line()) + theme(axis.line.x.top = element_line()) + guides(x.sec="axis") +  theme(axis.text.x.top = element_blank(), axis.ticks.x.top = element_blank()) + theme(axis.line.y.left = element_line()) + theme(axis.line.y.right = element_line()) + guides(y.sec="axis") +  theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) + labs(color = "Treatment") + theme(legend.key=element_rect(fill="white")) #Graphing the NMDS plot of modern species data. Note that this graph is not included in the publication and was generated for exploratory purposes.

species.scores <- as.data.frame(scores(mod.m1, "species"))  #Using the scores function from vegan to extract the species scores from the modern species data and converting the species scores to a data frame
species.scores <- na.omit(species.scores) #Removing NA values from species scores data frame for modern species data.
species.scores$species <- rownames(species.scores)  #Creating a column of species, from the rownames of species scores.
head(species.scores)  #Look at the data headers for the modern species scores.

scrs_mod.m1 <- scores(mod.m1, display = "sites", "species") #Extracting NMDS scores from modern species NMDS data frame.
cent_mod.m1 <- aggregate(scrs_mod.m1 ~ Treatment, data = mod.env, FUN = "mean") #Extracting centroids of NMDS scores from modern species from the modern species NMDS scores data frame.

ggplot() + stat_ellipse(data = NMDS.mod, geom = "polygon", aes(x = NMDS1, y = NMDS2, color = Treatment, fill = Treatment), alpha = 0.1, linewidth = 2, show.legend = FALSE) + geom_point(data = NMDS.mod, aes (x = NMDS1, y = NMDS2 , color = Treatment), size = 4) + geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5,segment.colour = NA) + theme(axis.title = element_text(size = 15, vjust = -4), text = element_text(size = 12.5)) + theme(panel.background = element_blank()) + theme(axis.line.x.bottom = element_line(),axis.line.x.top = element_line(), axis.text.x.top = element_blank(), axis.ticks.x.top = element_blank()) + theme(axis.line.y.left = element_line(), axis.line.y.right = element_line(), axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) + guides(y.sec="axis", x.sec="axis", size = FALSE) + theme(legend.key=element_rect(fill=NA)) + theme(legend.position="bottom") #Plotting NMDS comparison of treatment on modern species data displaying 95% confidence interval ellipses around the NMDS point cloud.

####### 
#### Read in modern species data reduced to only include species from McCormick & Somes (1982). 
#######

mod_som <- read.csv("C:/Users/brooksh/Dropbox (Smithsonian)/SeaGrant_Retrospective_2022/Dataverse_Upload/Modern_Somes.csv", header=TRUE, na.strings = "NA") #Reading in modern species data reduced to only include species from McCormick & Somes from my computer.
mod_som[is.na(mod_som)] <- 0 #Assigning value of 0 to missing data for cover class.
mod_som[mod_som == '1'] <- 2.5 #Assigning midpoint values of Braun-Blanquet cover classes.
mod_som[mod_som == '2'] <- 15 #Assigning midpoint values of Braun-Blanquet cover classes.
mod_som[mod_som == '3'] <- 37.5 #Assigning midpoint values of Braun-Blanquet cover classes.
mod_som[mod_som == '4'] <- 62.5 #Assigning midpoint values of Braun-Blanquet cover classes.
mod_som[mod_som == '5'] <- 87.5 #Assigning midpoint values of Braun-Blanquet cover classes.
summary(mod_som) #Looking at summary of data from NMDS matrix.
mod_som.nmds <- mod_som[-c(1:2)] #Removing first columns to make nmds matrix.
mod_som.env <- mod_som[-c(3:13)] #Removing last columns to make environmental matrix.

####### 
#### Do NMDS to modern species data reduced to only include species from McCormick & Somes (1982). By and large this is an exploratory look at the modern species data after reduction to the species described by McCormick and Somes (1982).
#######

mod_som.m1 <- metaMDS(mod_som.nmds, trymax = 100, trace = FALSE) #Doing NMDS to modern species data reduced to only include species from McCormick & Somes (1982).
mod_som.m1 #Checking the dimensions of the NMDS and the stress value. Note that the stress value is less than 0.2. Stress values do change each time the NMDS model is run.
stressplot(mod_som.m1) #Loking at stressplot of the NMDS to check how it looks.
NMDS.mod_som = data.frame(MDS1 = mod_som.m1$points[,1], MDS2 = mod_som.m1$points[,2],group=mod_som.env$Treatment) #Making the NMDS dataset for graphing using ggplot2.
ggplot(data = NMDS.mod_som, aes(MDS1, MDS2)) + geom_point(aes(color = group), size = 3) + scale_fill_brewer(palette = "Dark2") + theme(axis.title = element_text(size = 15, vjust = -4)) + theme(text = element_text(size = 12.5)) + theme(panel.background = element_blank()) + theme(axis.line.x.bottom = element_line()) + theme(axis.line.x.top = element_line()) + guides(x.sec="axis") +  theme(axis.text.x.top = element_blank(), axis.ticks.x.top = element_blank()) + theme(axis.line.y.left = element_line()) + theme(axis.line.y.right = element_line()) + guides(y.sec="axis") +  theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) + labs(color = "Treatment") + theme(legend.key=element_rect(fill="white")) #Graphing the NMDS plot of modern species data reduced to only include the species from McCormick & Somes (1982). Note that this graph is not included in the publication and was generated for exploratory purposes.

####### 
#### Read in modern species data reduced to only include the presence and absence of species from McCormick & Somes (1982).
#######

mod_som_presabs <- read.csv("C:/Users/brooksh/Dropbox (Smithsonian)/SeaGrant_Retrospective_2022/Dataverse_Upload/Modern_Somes_PresAbs.csv", header=TRUE, na.strings = "NA") #Reading in modern species data reduced to only include species from McCormick & Somes from my computer.
mod_som_presabs[is.na(mod_som_presabs)] <- 0 #Assigning value of 0 to missing data for cover class.
summary(mod_som_presabs) #Looking at summary of data from NMDS matrix.
mod_som_presabs.nmds <- mod_som_presabs[-c(1:2)] #Removing first columns to make nmds matrix.
mod_som_presabs.env <- mod_som_presabs[-c(3:13)] #Removing last columns to make environmental matrix.

####### 
#### Do NMDS to modern species data reduced to only include the presence and absence of species from McCormick & Somes (1982). By and large this is an exploratory look at the modern species data reduced to only include the presence and absence of species from McCormick & Somes (1982).
#######

mod_som_presabs.m1 <- metaMDS(mod_som_presabs.nmds, trymax = 100, trace = FALSE) #Doing NMDS to modern species data reduced to only include the presence and absence of species from McCormick & Somes (1982).
mod_som_presabs.m1 #Checking the dimensions of the NMDS and the stress value. Note that the stress value is less than 0.2. Stress values do change each time the NMDS model is run.
stressplot(mod_som_presabs.m1) #Loking at stressplot of the NMDS to check how it looks.
NMDS.mod_som_presabs = data.frame(MDS1 = mod_som_presabs.m1$points[,1], MDS2 = mod_som_presabs.m1$points[,2],group=mod_som_presabs.env$Treatment) #Making the NMDS dataset for graphing using ggplot2.
ggplot(data = NMDS.mod_som_presabs, aes(MDS1, MDS2)) + geom_point(aes(color = group), size = 3) + scale_fill_brewer(palette = "Dark2") + theme(axis.title = element_text(size = 15, vjust = -4)) + theme(text = element_text(size = 12.5)) + theme(panel.background = element_blank()) + theme(axis.line.x.bottom = element_line()) + theme(axis.line.x.top = element_line()) + guides(x.sec="axis") +  theme(axis.text.x.top = element_blank(), axis.ticks.x.top = element_blank()) + theme(axis.line.y.left = element_line()) + theme(axis.line.y.right = element_line()) + guides(y.sec="axis") +  theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) + labs(color = "Treatment") + theme(legend.key=element_rect(fill="white")) #Graphing the NMDS plot of modern species data reduced to only include the presence and absence of species from McCormick & Somes (1982). Note that this graph is not included in the publication and was generated for exploratory purposes.

####### 
#### Read in modern and historical species data reduced to only include presence/absence of species from McCormick & Somes (1982).
#######

mod_hist <- read.csv("C:/Users/brooksh/Dropbox (Smithsonian)/SeaGrant_Retrospective_2022/Dataverse_Upload/Combined_HistModern.csv", header=TRUE, na.strings = "NA") #Reading in modern and historical species data reduced to only include species from McCormick & Somes from my computer.
mod_hist[is.na(mod_hist)] <- 0 #Assigning value of 0 to missing data for cover class.
summary(mod_hist) #Looking at summary of data from NMDS matrix.
mod_hist.nmds <- mod_hist[-c(1:4)] #Removing first columns to make nmds matrix
mod_hist.env <- mod_hist[-c(5:16)] #Removing last columns to make environmental matrix
mod_hist.env$"Treatment:Time" <- paste(mod_hist.env$Treatment,"-",mod_hist.env$Time) #Making a column titled "Treatment:Time" in the environmental matrix combining treatment with the time.

####### 
#### Do NMDS to modern and historical species data reduced to only include presence/absence of species from McCormick & Somes (1982).
#######

mod_hist.m1 <- metaMDS(mod_hist.nmds, trymax = 100, trace = FALSE) #Doing NMDS to modern and historical species data reduced to only include the presence and absence of species from McCormick & Somes (1982).
mod_hist.m1 #Checking the dimensions of the NMDS and the stress value. Note that the stress value is less than 0.2. Stress values do change each time the NMDS model is run.
stressplot(mod_hist.m1)  #Loking at stressplot of the NMDS to check how it looks.
NMDS.mod_hist = data.frame(NMDS1 = mod_hist.m1$points[,1], NMDS2 = mod_hist.m1$points[,2], Treatment = mod_hist.env$Treatment, Time = mod_hist.env$Time, Treatment_Time = mod_hist.env$"Treatment:Time") #Making the NMDS dataset for graphing using ggplot2.
NMDS.mod_hist$"Treatment:Time" <- NMDS.mod_hist$Treatment.Time #Renaming column from "Treatment.Time" to "Treatment:Time."

species.scores.mod_hist <- as.data.frame(scores(mod_hist.m1, "species"))  #Using the scores function from vegan to extract the modern and historical species scores and convert to a data.frame.
species.scores.mod_hist$species <- rownames(species.scores.mod_hist)  #Create a column of species, from the rownames of species.scores
head(species.scores.mod_hist)  #Look at the data.

scrs_mod_hist.m1 <- scores(mod_hist.m1, display = "sites", "species") #Extracting NMDS scores from modern and historical species NMDS data frame.
cent_mod_hist.m1 <- aggregate(scrs_mod_hist.m1 ~ Treatment + Time, data = mod_hist.env, FUN = "mean") #Extracting centroids of NMDS scores from modern and historical species NMDS data frame.

ggplot(data = NMDS.mod_hist, aes(NMDS1, NMDS2)) + geom_jitter(aes(color = Time), size = 2.5, width = 0.1, height = 0.1) + scale_color_manual(values=c("#69b3a2", "purple")) + theme(axis.title = element_text(size = 15, vjust = -4)) + theme(text = element_text(size = 12.5)) + theme(panel.background = element_blank()) + theme(axis.line.x.bottom = element_line()) + theme(axis.line.x.top = element_line()) + guides(x.sec="axis") +  theme(axis.text.x.top = element_blank(), axis.ticks.x.top = element_blank()) + theme(axis.line.y.left = element_line()) + theme(axis.line.y.right = element_line()) + guides(y.sec="axis") +  theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) + labs(color = "Time") + theme(legend.key=element_rect(fill="white")) #Graphing the NMDS plot of modern and historical species data reduced to only include the presence and absence of species from McCormick & Somes (1982).  Note that this graph is not included in the publication and was generated for exploratory purposes.

ggplot() + stat_ellipse(data = NMDS.mod_hist, geom = "polygon", aes(x = NMDS1, y = NMDS2, color = Treatment, fill = Treatment), alpha = 0.1, size = 2, show.legend = FALSE) + geom_point(data = cent_mod_hist.m1, aes (x = NMDS1, y = NMDS2 , color = Treatment, shape = Time), size = 6) + theme(axis.title = element_text(size = 15, vjust = -4), text = element_text(size = 12.5)) + theme(panel.background = element_blank()) + theme(axis.line.x.bottom = element_line(),axis.line.x.top = element_line(), axis.text.x.top = element_blank(), axis.ticks.x.top = element_blank()) + theme(axis.line.y.left = element_line(), axis.line.y.right = element_line(), axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) + guides(y.sec="axis", x.sec="axis", size = FALSE) + theme(legend.key=element_rect(fill=NA)) + theme(legend.position="bottom") #Graphing the NMDS plot of modern and historical species data reduced to only include the centroids of treatment and time and the 95% confidence interval ellipses.  Note that this graph is not included in the publication and was generated for exploratory purposes.

ggplot() + stat_ellipse(data = NMDS.mod_hist, geom = "polygon", aes(x = NMDS1, y = NMDS2, color = Treatment_Time, fill = Treatment_Time), alpha = 0.1, size = 2, show.legend = FALSE) + geom_point(data = NMDS.mod_hist, aes (x = NMDS1, y = NMDS2 , color = Treatment_Time), size = 4) + geom_text(data=species.scores.mod_hist,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) + guides(color=guide_legend(title="Treatment:Time")) + theme(axis.title = element_text(size = 15, vjust = -4), text = element_text(size = 12.5)) + theme(panel.background = element_blank()) + theme(axis.line.x.bottom = element_line(),axis.line.x.top = element_line(), axis.text.x.top = element_blank(), axis.ticks.x.top = element_blank()) + theme(axis.line.y.left = element_line(), axis.line.y.right = element_line(), axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) + guides(y.sec="axis", x.sec="axis", size = FALSE) + theme(legend.key=element_rect(fill=NA)) + theme(legend.position="bottom") #Plotting NMDS comparison of treatment and time with all points and species codes plotted and ellipses encapsulating each unique Treatment:Time combination.

####### 
#### Removing NMDS plots and datasets from R environment.
#######

rm(hist.env, hist.m1, hist.nmds, mod_hist.m1, mod_som_presabs.env, mod_som_presabs.m1, mod_som_presabs.nmds, mod_som.env, mod_som.m1, mod_som.nmds, mod.m1, NMDS.hist, NMDS.mod, NMDS.mod_hist, NMDS.mod_som, NMDS.mod_som_presabs,cent_mod_hist.m1, scrs_mod_hist.m1, scrs_mod.m1, cent_mod.m1)

####### 
#### PERMANOVA comparing treatment, time, and treatment x time interactions in historical and modern presence-absence dataset.
#######

adonis2(mod_hist.nmds ~ Treatment*Time, mod_hist.env, perm = 999) #Running PERMANOVA on treatment and time in the modern and historical presence-absence dataset.


####### 
#### PERMANOVA comparing treatment effects on modern community cover dataset.
#######

adonis2(mod.nmds ~ Treatment, mod.env, perm = 999) #Running PERMANOVA on treatment in the modern species cover dataset.

####### 
#### ANOVA comparing treatment effects on species richness on modern community cover dataset.
#######

mod.env$Spp_Rich <- as.numeric (mod.env$Spp_Rich) #Making species richness numeric
shapiro.test(mod.env$Spp_Rich) #Testing assumptions of normality, fails assumptions
leveneTest(Spp_Rich ~ Treatment, data = mod.env) #Levene testing, passes
mod <- aov(Spp_Rich ~ Treatment, mod.env) #Running ANOVA because ANOVA is robust against non-normality
summary(mod) #Looking at the results of the ANOVA.

####### 
#### Calculating mean and SE species richness from modern community cover dataset.
#######
modmeans <- aggregate(mod.env$Spp_Rich, by=list(mod.env$Treatment), na.rm = TRUE, FUN = mean) #Calculate treatment x species richness means
modmeans <-as.data.frame(modmeans) #Make treatment and species richness means into a dataframe
colnames(modmeans) <- c("Treatment", "Means") #Rename columns in treatment x species richness dataframe

modse <- aggregate(mod.env$Spp_Rich, by=list(mod.env$Treatment), na.rm = TRUE, FUN = std.error) #Calculate treatment x species richness std. errors
modse <- as.data.frame(modse) #Make treatment species richness standard errors into a dataframe
colnames(modse) <- c("Treatment", "SE") #Rename columns in treatment x species richness dataframe
mod_spp_rich_Means_SE <- inner_join(x = modmeans, y = modse, by = "Treatment") #Join means and SE
write.csv(mod_spp_rich_Means_SE,paste(path_out,'mod_spp_rich_Means_SE',sep = '')) #Write out .csv file

####### 
#### Plotting species richness.
#######

plot_spp_rich <- ggplot(data = mod.env, aes(Treatment, Spp_Rich)) #Defining plot for treatment and species richness
plot_spp_rich + geom_violin(alpha = 0.5, trim = FALSE, fill = "grey80", lwd = 0.75) + ylab("Species Richness") + xlab("Treatment") + geom_boxplot(width = 0.2, size = 0.75) + stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) + stat_summary(fun = "mean",geom = "point", shape = 21, fill = "blue", size = 4) + scale_fill_brewer(palette = "Dark2") + theme(axis.title = element_text(size = 15, vjust = -4)) + theme(text = element_text(size = 12.5)) + theme(legend.position = "none") + theme(axis.line = element_line()) + theme(panel.background = element_blank()) #Making violin plots of species richness differences between treatments.

####### 
#### ANOVA comparing treatment effects on Phragmites cover in modern community cover dataset.
#######

mod.env$Phrag_Cover <- as.numeric (mod.env$Phrag_Cover) #Making Phragmites cover numeric
mod.env$Treatment <- as.factor(mod.env$Treatment) #Making Treatment a factor
shapiro.test(mod.env$Phrag_Cover) #Testing assumptions of normality, fails assumptions
leveneTest(Phrag_Cover ~ Treatment, mod.env) #Testing assumptions of homogeneity of variance, also fails
hist(mod.env$Phrag_Cover) #Looking at histogram
mod1 <- welch_anova_test(mod.env, Phrag_Cover ~ Treatment) #Running Welch's ANOVA on Phragmites cover because it is robust against failed normality and and heterogeneous variance
mod1 #Checking results of ANOVA on Phragmites cover

####### 
#### Calculating mean and SE Phragmites percent cover from modern community cover dataset.
#######
modphrag <- aggregate(mod.env$Phrag_Cover, by=list(mod.env$Treatment), na.rm = TRUE, FUN = mean) #Calculate treatment x Phragmites means
modphrag <-as.data.frame(modphrag) #Make treatment and Phragmites means into a dataframe
colnames(modphrag) <- c("Treatment", "Means") #Rename columns in treatment x Phragmites means

modphragse <- aggregate(mod.env$Spp_Rich, by=list(mod.env$Treatment), na.rm = TRUE, FUN = std.error) #Calculate treatment x Phragmites SE
modphragse <- as.data.frame(modphragse) #Make treatment Phragmites standard errors into a dataframe
colnames(modphragse) <- c("Treatment", "SE") #Rename columns in treatment x Phragmites cover SE dataframe
mod_Phrag_Means_SE <- inner_join(x = modphrag, y = modphragse, by = "Treatment") #Join means and SE
write.csv(mod_Phrag_Means_SE,paste(path_out,'mod_Phrag_Means_SE',sep = '')) #Write out .csv file

####### 
#### Plotting Phragmites percent cover
#######

plot_Phrag <- ggplot(data = mod.env, aes(Treatment, Phrag_Cover)) #Defining plot for treatment and Phragmites cover
plot_Phrag + geom_violin(alpha = 0.5, trim = FALSE, fill = "grey80", lwd = 0.75) + ylab(expression("Percent Cover" ~ italic("Phragmites"))) + xlab("Treatment") + geom_boxplot(width = 0.2, size = 0.75) + stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) + stat_summary(fun = "mean",geom = "point", shape = 21, fill = "blue", size = 4) + scale_fill_brewer(palette = "Dark2") + theme(axis.title = element_text(size = 15, vjust = -4)) + theme(text = element_text(size = 12.5)) + theme(legend.position = "none") + theme(axis.line = element_line()) + theme(panel.background = element_blank()) #Making violin plots of Phragmites cover differences between treatments.


