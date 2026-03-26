
# Whole genome sequencing of two human rhinovirus A types (A101 and A15) detected in Kenya, 2016-2018
## Martha Luka
# 04/03/2021

#Set working directory

mydata<- read.csv("spred.csv", header = T)
head(mydata)

require(pacman)
pacman::p_load(ggplot2,dplyr, ggpubr, ggbeeswarm)


#Figure 1A#############
##Ct-value distribution across all samples
ggplot(mydata, aes(x=ct_value, fill=type, color="black"))+geom_histogram() +
  scale_y_continuous(name="Frequency", breaks = seq(0,10, 2)) + 
  scale_x_continuous(name = "Cycle threshold", 
                     breaks = seq(15, 40, 5), limits = c(15, 40)) +
  scale_fill_manual(values = c("A101" = "darkgoldenrod1",  "A15"="turquoise4"))+
  scale_colour_manual(values = c("black"))

##Figure 1B##############
#First, check normality of data using Shapiro-Wilk normality test for samples that PASS versus FAIL sequencing
#Assumption 1 (the two groups are independent)- yes the two groups are independent (failed versus passed)

#Assumption 2: Are the data from each of the 2 groups follow a normal distribution?
  ###2a Shapiro-Wilk normality test for PASS ct values
with(mydata, shapiro.test(ct_value[sequencing == "PASS"])) # p = 0.7341

  ###2b Shapiro-Wilk normality test for FAIL ct values
with(mydata, shapiro.test(ct_value[sequencing == "FAIL"])) # p = 0.6061 

#p-values are greater than the significance level 0.05 
#implying that the distribution of the data are not significantly different from the normal distribution.
#We can therefore assume normality

#Assumption 3 Do the two populations have the same variances?.... F-test to test for homogeneity in variances
var.test(ct_value ~ sequencing, data = mydata) 
    # p=0.6018 implying that there is no significant differences in variance of the two groups

#4.we now use ttest independent ttest of equal variances since all the assumptions have been met
 t.test(ct_value ~ sequencing, data = mydata, var.equal = TRUE) 
    # p=0.123 implying that there is no significant difference between

# getting the median of cycle threshold value by sequencing group(pass,fail) to use in boxplot
medians <- aggregate(ct_value~sequencing, mydata, median)


#Figure 1B
mydata %>% 
  mutate(sequencing = factor(sequencing, levels = c("PASS", "FAIL"), ordered = T)) %>% 
  ggplot(aes(sequencing, ct_value, show.legend =F))+
  geom_boxplot(show.legend = F)+
  geom_quasirandom(aes(col = sequencing), stat = "identity", position = position_jitterdodge(jitter.width = 0.1))+
  stat_compare_means(method = "t.test",method.args = list(var.equal = TRUE),label.y = 36)+
  theme_classic()+
  theme(axis.text.y = element_text(face = "bold", size = 11), 
        axis.text.x = element_text(face = "bold", size = 11), 
        legend.text = element_text(size = 11, face = "bold"))+
  theme(axis.title.x = element_text(size = 15, face = "bold"), 
        axis.title.y = element_text(size = 15, face = "bold"), 
        legend.title = element_text(face = "bold"))+
  xlab("Sequencing")+
  ylab("Cycle threshold value") + 
  geom_text(data = medians, aes(y = ct_value, label = round(ct_value,2)),size = 5, vjust = -0.3, color='black')


#Figure 1C is generated from bam files of raw sequence reads using 
    #the package deepTools (https://deeptools.readthedocs.io/en/develop/content/installation.html)

#Figure 1D############
##Distribution of mean coverage per genome#######
mydata1<- mydata[mydata$sequencing=="PASS",] #only samples successfully sequenced

ggplot(mydata1, aes(meanCoveragepergenome, fill= ct_group)) +
  geom_histogram(binwidth=500) + theme_minimal() + 
  scale_fill_manual(values = c("20-24" = "salmon3",  "25-29"="gray64",
                               "30-34"="turquoise4")) +
  labs(y = "Count", x = "Mean coverage per genome", fill = "Ct value") +
  theme_classic() + theme_bw() +
  theme(axis.title = element_text(size = 20, face="bold"),
        axis.text.x = element_text(size=17, face="bold"),
        axis.text.y =element_text(size=17, face="bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size=18, face="bold"))+
  scale_x_continuous(breaks = c(0, 2500, 5000, 7500, 10000, 12500))


