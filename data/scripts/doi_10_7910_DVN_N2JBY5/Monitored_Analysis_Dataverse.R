#install.packages(c('ggplot2','vegan','betapart','BiodiversityR','dplyr','lme4','rstatix','ggordiplots','ggforce','car')) #Remove hash to install packages necessary for analysis. I have included the hash to make this a comment and not consistently re-download packages as I run this analysis.

library(ggplot2);library(vegan);library(betapart);library(BiodiversityR);library(dplyr);library(lme4);library(ggordiplots);library(ggforce);library(car) #This line of code calls the packages up.

std.error <- function(x, na.rm=FALSE) {if(na.rm==TRUE) x <- na.omit(x)
sd(x)/sqrt(length(x))
} #Defining standard error to calculate it

path_out = 'C:/Users/brooksh/Dropbox (Smithsonian)/SeaGrant_Retrospective_2022/Dataverse_Upload/' #This is defining the path to save various small dataframes to as .csv files. Change this to your preferred folder configuration.

####### 
#### Comparing treatment effects on species richness and Phragmites cover in 2011.
#######
df2011 <- read.csv("C:/Users/brooksh/Dropbox (Smithsonian)/SeaGrant_Retrospective_2022/TMON/NMDS_Datasets/2011_TMON_All.csv", header=TRUE) #Reading in dataset of 2011 plant cover classes, species richness, and Phragmites cover.

#ANOVA comparing treatment effects on species richness in 2011.
df2011$Treatment <- as.factor(df2011$Treatment) #Making treatment a factor for analysis purposes
shapiro.test(df2011$SpRich) #Using a Shapiro-Wilk test to check assumptions of normality. Assumptions of normality failed (W = 0.86278, p-value = 3.701e-09).
leveneTest(SpRich ~ Treatment, df2011) #Using Levene's test to check the assumption of homogeneity of variance. Assumptions of homogeneity of variance passed (F = 0.2462, p-value = 0.7822).
rich2011 <- aov(SpRich ~ Treatment, df2011) #Running ANOVA on species richness x t
summary(rich2011)

#Calculating treatment mean and SE species richness from 2011.
means2011 <- aggregate(df2011$SpRich, by=list(df2011$Treatment), na.rm = TRUE, FUN = mean) #Calculate treatment x species richness means for 2011
colnames(means2011) <- c("Treatment", "Means") #Rename columns in treatment x  species richness dataframe for 2011

se2011 <- aggregate(df2011$SpRich, by=list(df2011$Treatment), na.rm = TRUE, FUN = std.error) #Calculate treatment x species richness standard errors for 2011
colnames(se2011) <- c("Treatment", "SE") #Rename columns in treatment x species richness standard error dataframe for 2011
Rich_Means_SE_2011 <- inner_join(x = means2011, y = se2011, by = "Treatment") #Join means and standard errors
Rich_Means_SE_2011$Year <- c(2011,2011,2011)
write.csv(Rich_Means_SE_2011,paste(path_out,'Rich_Means_SE_2011',sep = '')) #Write out .csv file of 2011 species richness means and standard errors

#Calculating Tukey-HSD between treatment species richness from 2011.
tukey2011 <- TukeyHSD(aov(SpRich ~ Treatment, data = df2011)) #Running Tukey test on the 2011 species richness differences between treatments
tukey2011 <-as.data.frame(tukey2011[1:1]) #Making dataframe of Tukey test results
tukey2011 <- tukey2011[-c(1:3)] #Removing first columns of Tukey test result dataframe (first columns include treatment differences, lower, and upper limits)
colnames(tukey2011) <- c("p-Values") #Renaming p-value column
write.csv(tukey2011,paste(path_out,'Rich_Tukey2011',sep = '')) #Write out .csv file of Tukey test results for 2011 species richness

#Plotting species richness for different treatments from 2011.
plot_rich_2011 <- ggplot(data = df2011, aes(Treatment, SpRich)) #Defining plot for treatment and native species richness
plot_rich_2011 + geom_violin(trim = FALSE, color = NA, aes(fill = Treatment)) + ylab("Species Richness") + xlab("Treatment") + geom_boxplot(width = 0.15, size = 0.5) + stat_summary(fun = "mean",geom = "point", shape = 24, fill = "black", size = 4) + theme(axis.title = element_text(size = 15, vjust = -4)) + theme(text = element_text(size = 12.5)) + theme(legend.position = "none") + theme(axis.line = element_line()) + theme(panel.background = element_blank()) #Graphing violin plot for 2011 species richness

rm(means2011, plot_rich_2011, rich2011, se2011, tukey2011)

#ANOVA comparing treatment effects on Phragmites cover in 2011.
df2011$Treatment <- as.factor(df2011$Treatment)  #Making treatment a factor for analysis purposes
shapiro.test(df2011$PHAU) ##Using a Shapiro-Wilk test to check assumptions of normality. Assumptions of normality failed (W = 0.67704, p-value = 6.85e-15).
leveneTest(PHAU ~ Treatment, df2011) #Using Levene's test to check the assumption of homogeneity of variance. Assumptions of homogeneity of variance failed, marginally (F = 4.7204, p-value = 0.01068).
phrag2011 <- welch_anova_test(df2011, PHAU ~ Treatment)
phrag2011

#Calculating treatment mean and SE species Phragmites cover from 2011.
means2011 <- aggregate(df2011$PHAU, by=list(df2011$Treatment), na.rm = TRUE, FUN = mean) #Calculate treatment x Phragmites cover means for 2011
colnames(means2011) <- c("Treatment", "Means") #Rename columns in treatment x  Phragmites cover means dataframe for 2011

se2011 <- aggregate(df2011$PHAU, by=list(df2011$Treatment), na.rm = TRUE, FUN = std.error) #Calculate treatment x Phragmites cover std. errors for 2011
colnames(se2011) <- c("Treatment", "SE") #Rename columns in treatment x Phragmites std. error dataframe for 2011
Phrag_Means_SE_2011 <- inner_join(x = means2011, y = se2011, by = "Treatment") #Join means and SE
Phrag_Means_SE_2011$Year <- c(2011,2011,2011)
write.csv(Phrag_Means_SE_2011,paste(path_out,'Phrag_Means_SE_2011',sep = '')) #Write out .csv file

#Calculating Tukey-HSD between treatment PHragmites cover from 2011.
tukey2011 <- TukeyHSD(aov(PHAU ~ Treatment, data = df2011)) #Running Tukey test
tukey2011 <-as.data.frame(tukey2011[1:1]) #Making dataframe of Tukey test
tukey2011 <- tukey2011[-c(1:3)] #Removing first columns
colnames(tukey2011) <- c("p-Values") #Renaming columns
write.csv(tukey2011,paste(path_out,'Phrag_Tukey2011',sep = '')) #Write out .csv file

#Plotting species richness for different treatments from 2011.
plot_Phrag_2011 <- ggplot(data = df2011, aes(Treatment, PHAU)) #Defining plot for treatment and native species richness
plot_Phrag_2011 + geom_violin(color = NA, aes(fill = Treatment)) + ylab("Phragmites % Cover") + xlab("Treatment") + geom_boxplot(width = 0.15, size = 0.5) + stat_summary(fun = "mean",geom = "point", shape = 24, fill = "black", size = 4) + theme(axis.title = element_text(size = 15, vjust = -4)) + theme(text = element_text(size = 12.5)) + theme(legend.position = "none") + theme(axis.line = element_line()) + theme(panel.background = element_blank()) #Adding the doodads

rm(df2011, means2011, plot_Phrag_2011, se2011, tukey2011, phrag2011)

####### 
#### Comparing treatment effects on species richness and Phragmites cover in 2014.
#######
df2014 <- read.csv("C:/Users/brooksh/Dropbox (Smithsonian)/SeaGrant_Retrospective_2022/TMON/NMDS_Datasets/2014_TMON_All.csv", header=TRUE) #Reading dataset of outcomes

#ANOVA comparing treatment effects on species richness in 2014.
df2014$Treatment <- as.factor(df2014$Treatment)
shapiro.test(df2014$SpRich) #Assumptions of normality failed
leveneTest(SpRich ~ Treatment, df2014) #Assumption of homogeneity of variance passed
rich2014 <- aov(SpRich ~ Treatment, df2014)
summary(rich2014)

#Calculating treatment mean and SE species richness from 2014.
means2014 <- aggregate(df2014$SpRich, by=list(df2014$Treatment), na.rm = TRUE, FUN = mean) #Calculate treatment x species richness means for 2014
colnames(means2014) <- c("Treatment", "Means") #Rename columns in treatment x  species richness dataframe for 2014

se2014 <- aggregate(df2014$SpRich, by=list(df2014$Treatment), na.rm = TRUE, FUN = std.error) #Calculate treatment x species richness std. errors for 2014
colnames(se2014) <- c("Treatment", "SE") #Rename columns in treatment x species richness dataframe for 2014
Rich_Means_SE_2014 <- inner_join(x = means2014, y = se2014, by = "Treatment") #Join means and SE
Rich_Means_SE_2014$Year <- c(2014,2014,2014)
write.csv(Rich_Means_SE_2014,paste(path_out,'Rich_Means_SE_2014',sep = '')) #Write out .csv file

#Calculating Tukey-HSD between treatment species richness from 2014.
tukey2014 <- TukeyHSD(aov(SpRich ~ Treatment, data = df2014)) #Running Tukey test
tukey2014 <-as.data.frame(tukey2014[1:1]) #Making dataframe of Tukey test
tukey2014 <- tukey2014[-c(1:3)] #Removing first columns
colnames(tukey2014) <- c("p-Values") #Renaming columns
write.csv(tukey2014,paste(path_out,'Rich_Tukey2014',sep = '')) #Write out .csv file

#Plotting species richness for different treatments from 2014.
plot_rich_2014 <- ggplot(data = df2014, aes(Treatment, SpRich)) #Defining plot for treatment and native species richness
plot_rich_2014 + geom_violin(trim = FALSE, color = NA, aes(fill = Treatment)) + ylab("Species Richness") + xlab("Treatment") + geom_boxplot(width = 0.15, size = 0.5) + stat_summary(fun = "mean",geom = "point", shape = 24, fill = "black", size = 4) + theme(axis.title = element_text(size = 15, vjust = -4)) + theme(text = element_text(size = 12.5)) + theme(legend.position = "none") + theme(axis.line = element_line()) + theme(panel.background = element_blank()) #Adding the doodads

rm(means2014, plot_rich_2014, rich2014, se2014, tukey2014)

#ANOVA comparing treatment effects on Phragmites cover in 2014.
df2014$Treatment <- as.factor(df2014$Treatment)
shapiro.test(df2014$PHAU) #Assumptions of normality failed
leveneTest(PHAU ~ Treatment, df2014) #Assumption of homogeneity of variance failed
phrag2014 <- welch_anova_test(df2014, PHAU ~ Treatment)
phrag2014

#Calculating treatment mean and SE species Phragmites cover from 2014.
means2014 <- aggregate(df2014$PHAU, by=list(df2014$Treatment), na.rm = TRUE, FUN = mean) #Calculate treatment x Phragmites cover means for 2014
colnames(means2014) <- c("Treatment", "Means") #Rename columns in treatment x  Phragmites cover means dataframe for 2014

se2014 <- aggregate(df2014$PHAU, by=list(df2014$Treatment), na.rm = TRUE, FUN = std.error) #Calculate treatment x Phragmites cover std. errors for 2014
colnames(se2014) <- c("Treatment", "SE") #Rename columns in treatment x Phragmites std. error dataframe for 2014
Phrag_Means_SE_2014 <- inner_join(x = means2014, y = se2014, by = "Treatment") #Join means and SE
Phrag_Means_SE_2014$Year <- c(2014,2014,2014)
write.csv(Phrag_Means_SE_2014,paste(path_out,'Phrag_Means_SE_2014',sep = '')) #Write out .csv file

#Calculating Tukey-HSD between treatment PHragmites cover from 2014.
tukey2014 <- TukeyHSD(aov(PHAU ~ Treatment, data = df2014)) #Running Tukey test
tukey2014 <-as.data.frame(tukey2014[1:1]) #Making dataframe of Tukey test
tukey2014 <- tukey2014[-c(1:3)] #Removing first columns
colnames(tukey2014) <- c("p-Values") #Renaming columns
write.csv(tukey2014,paste(path_out,'Phrag_Tukey2014',sep = '')) #Write out .csv file

#Plotting species richness for different treatments from 2014.
plot_Phrag_2014 <- ggplot(data = df2014, aes(Treatment, PHAU)) #Defining plot for treatment and native species richness
plot_Phrag_2014 + geom_violin(color = NA, aes(fill = Treatment)) + ylab("Phragmites % Cover") + xlab("Treatment") + geom_boxplot(width = 0.15, size = 0.5) + stat_summary(fun = "mean",geom = "point", shape = 24, fill = "black", size = 4) + theme(axis.title = element_text(size = 15, vjust = -4)) + theme(text = element_text(size = 12.5)) + theme(legend.position = "none") + theme(axis.line = element_line()) + theme(panel.background = element_blank()) #Adding the doodads

rm(df2014, means2014, plot_Phrag_2014, se2014, tukey2014, phrag2014)

####### 
#### ANOVA comparing treatment effects on species richness and Phragmites cover in 2020.
#######
df2020 <- read.csv("C:/Users/brooksh/Dropbox (Smithsonian)/SeaGrant_Retrospective_2022/TMON/NMDS_Datasets/2020_TMON_All.csv", header=TRUE) #Reading dataset of outcomes

#ANOVA comparing treatment effects on species richness in 2020.
df2020$Treatment <- as.factor(df2020$Treatment)
shapiro.test(df2020$SpRich) #Assumptions of normality failed
leveneTest(SpRich ~ Treatment, df2020) #Assumption of homogeneity of variance failed
rich2020 <- welch_anova_test(df2020, SpRich ~ Treatment)
rich2020

#Calculating treatment mean and SE species richness from 2020.
means2020 <- aggregate(df2020$SpRich, by=list(df2020$Treatment), na.rm = TRUE, FUN = mean) #Calculate treatment x species richness means for 2020
colnames(means2020) <- c("Treatment", "Means") #Rename columns in treatment x  species richness dataframe for 2020

se2020 <- aggregate(df2020$SpRich, by=list(df2020$Treatment), na.rm = TRUE, FUN = std.error) #Calculate treatment x species richness std. errors for 2020
colnames(se2020) <- c("Treatment", "SE") #Rename columns in treatment x species richness dataframe for 2020
Rich_Means_SE_2020 <- inner_join(x = means2020, y = se2020, by = "Treatment") #Join means and SE
Rich_Means_SE_2020$Year <- c(2020,2020,2020)
write.csv(Rich_Means_SE_2020,paste(path_out,'Rich_Means_SE_2020',sep = '')) #Write out .csv file

#Calculating Tukey-HSD between treatment species richness from 2020.
tukey2020 <- TukeyHSD(aov(SpRich ~ Treatment, data = df2020)) #Running Tukey test
tukey2020 <-as.data.frame(tukey2020[1:1]) #Making dataframe of Tukey test
tukey2020 <- tukey2020[-c(1:3)] #Removing first columns
colnames(tukey2020) <- c("p-Values") #Renaming columns
write.csv(tukey2020,paste(path_out,'Rich_Tukey2020',sep = '')) #Write out .csv file

#Plotting species richness for different treatments from 2020.
plot_rich_2020 <- ggplot(data = df2020, aes(Treatment, SpRich)) #Defining plot for treatment and native species richness
plot_rich_2020 + geom_violin(trim = FALSE, color = NA, aes(fill = Treatment)) + ylab("Species Richness") + xlab("Treatment") + geom_boxplot(width = 0.15, size = 0.5) + stat_summary(fun = "mean",geom = "point", shape = 24, fill = "black", size = 4) + theme(axis.title = element_text(size = 15, vjust = -4)) + theme(text = element_text(size = 12.5)) + theme(legend.position = "none") + theme(axis.line = element_line()) + theme(panel.background = element_blank()) #Adding the doodads

rm(means2020, plot_rich_2020, rich2020, se2020, tukey2020)

#ANOVA comparing treatment effects on Phragmites cover in 2020.
df2020$Treatment <- as.factor(df2020$Treatment)
shapiro.test(df2020$PHAU) #Assumptions of normality failed
leveneTest(PHAU ~ Treatment, df2011) #Assumption of homogeneity of variance failed
phrag2020 <- welch_anova_test(df2020, PHAU ~ Treatment)
phrag2020

#Calculating treatment mean and SE species Phragmites cover from 2020.
means2020 <- aggregate(df2020$PHAU, by=list(df2020$Treatment), na.rm = TRUE, FUN = mean) #Calculate treatment x Phragmites cover means for 2020
colnames(means2020) <- c("Treatment", "Means") #Rename columns in treatment x  Phragmites cover means dataframe for 2020

se2020 <- aggregate(df2020$PHAU, by=list(df2020$Treatment), na.rm = TRUE, FUN = std.error) #Calculate treatment x Phragmites cover std. errors for 2020
colnames(se2020) <- c("Treatment", "SE") #Rename columns in treatment x Phragmites std. error dataframe for 2020
Phrag_Means_SE_2020 <- inner_join(x = means2020, y = se2020, by = "Treatment") #Join means and SE
Phrag_Means_SE_2020$Year <- c(2020,2020,2020)
write.csv(Phrag_Means_SE_2020,paste(path_out,'Phrag_Means_SE_2020',sep = '')) #Write out .csv file

#Calculating Tukey-HSD between treatment PHragmites cover from 2020.
tukey2020 <- TukeyHSD(aov(PHAU ~ Treatment, data = df2020)) #Running Tukey test
tukey2020 <-as.data.frame(tukey2020[1:1]) #Making dataframe of Tukey test
tukey2020 <- tukey2020[-c(1:3)] #Removing first columns
colnames(tukey2020) <- c("p-Values") #Renaming columns
write.csv(tukey2020,paste(path_out,'Phrag_Tukey2020',sep = '')) #Write out .csv file

#Plotting species richness for different treatments from 2020.
plot_Phrag_2020 <- ggplot(data = df2020, aes(Treatment, PHAU)) #Defining plot for treatment and native species richness
plot_Phrag_2020 + geom_violin(color = NA, aes(fill = Treatment)) + ylab("Phragmites % Cover") + xlab("Treatment") + geom_boxplot(width = 0.15, size = 0.5) + stat_summary(fun = "mean",geom = "point", shape = 24, fill = "black", size = 4) + theme(axis.title = element_text(size = 15, vjust = -4)) + theme(text = element_text(size = 12.5)) + theme(legend.position = "none") + theme(axis.line = element_line()) + theme(panel.background = element_blank()) #Adding the doodads

rm(df2020, means2020, plot_Phrag_2020, se2020, tukey2020, phrag2020)

#######
## Graphing species richness for 2011, 2014, and 2020.
#######
rich_compile <- bind_rows(Rich_Means_SE_2011,Rich_Means_SE_2014,Rich_Means_SE_2020)
rich_compile$Year <- as.factor(rich_compile$Year)

rich_compile_plot <- ggplot(data = rich_compile, aes(Treatment, Means, fill = Treatment)) #Defining plot for treatment and native species richness
rich_compile_plot + geom_bar(stat = "identity", position = "dodge") + geom_errorbar(aes(ymin = Means - SE, ymax = Means + SE), width = 0.2, size = 1) + facet_grid(~ Year, scales = "free_x", space = "free_x", switch = "x") + ylab("Species Richness") + xlab("Treatment") + theme(axis.title = element_text(size = 15, vjust = -4)) + theme(text = element_text(size = 13)) + theme(axis.line = element_line(size = 1)) + theme(panel.background = element_blank()) + theme(legend.position = "none") + theme(strip.background = element_rect(fill = "white")) + theme(strip.text.x = element_text(size = 12, face = "italic"))

#######
## Graphing Phragmites cover for 2011, 2014, and 2020.
#######
Phrag_compile <- bind_rows(Phrag_Means_SE_2011,Phrag_Means_SE_2014,Phrag_Means_SE_2020)
Phrag_compile$Year <- as.factor(Phrag_compile$Year)

phrag_compile_plot <- ggplot(data = Phrag_compile, aes(Treatment, Means, fill = Treatment)) #Defining plot for treatment and native species richness
phrag_compile_plot + geom_bar(stat = "identity", position = "dodge") + geom_errorbar(aes(ymin = Means - SE, ymax = Means + SE), width = 0.2, size = 1) + facet_grid(~ Year, scales = "free_x", space = "free_x", switch = "x") + ylab(expression("Percent Cover" ~ italic("Phragmites"))) + xlab("Treatment") + theme(axis.title = element_text(size = 15, vjust = -4)) + theme(text = element_text(size = 13)) + theme(axis.line = element_line(size = 1)) + theme(panel.background = element_blank()) + theme(legend.position = "none") + theme(strip.background = element_rect(fill = "white")) + theme(strip.text.x = element_text(size = 12, face = "italic"))

#######
## Regressing Phragmites cover versus species richness for 2011, 2014, and 2020.
#######
df2011$SpRich2011 <- df2011$SpRich
df2011$PHAU2011 <- df2011$PHAU
indices_to_remove <- seq(46, 60)
df2011 <- df2011[-indices_to_remove, ]
df2011 <- df2011[-c(10,71,72), ]
indices_to_remove2 <- seq(76, 90)
df2011 <- df2011[-indices_to_remove2, ]

df2014$SpRich2014 <- df2014$SpRich
df2014$PHAU2014 <- df2014$PHAU
df2020$SpRich2020 <- df2020$SpRich
df2020$PHAU2020 <- df2020$PHAU

regression_compile <- rbind(df2011$SpRich2011, df2011$PHAU2011, df2014$SpRich2014, df2014$PHAU2014, df2020$SpRich2020, df2020$PHAU2020)
