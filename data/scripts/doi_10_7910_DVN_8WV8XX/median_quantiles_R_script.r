#plotting boxplots of the response variables
#written by T Revathe

rm(list=ls())

#reading and wrangling data
#--------------------------

xdata=read.table(file="median_quantiles.txt", header=T,
                 stringsAsFactors=T, sep="\t")
str(xdata)
names(xdata)

# Convert data frame to matrix
data_matrix <- as.matrix(xdata)

# Create horizontal boxplot
dev.new()

par(mar=c(3, 3, 0.2, 0.2)) #setting margin widths for figure 
par(mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1) #setting where axis labels are displayed

boxplot(data_matrix, horizontal = TRUE, xlab = "Probability that mother shows a behaviour")