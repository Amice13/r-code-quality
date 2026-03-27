################################################################################
# Replication archive for
# Bechtel, Michael M./Liesch, Roman/Scheve, Kenneth F. (2018) 
# "Inequality and Redistribution Behavior in a Give-or-Take Game"
# Proceedings of the National Academy of Sciences of 
# the United States of America
# Available at: XXX
################################################################################

rm(list=ls(all=TRUE))

install.packages("ggplot2")
install.packages("readstata13")
install.packages("scales")
install.packages("cowplot")

library(ggplot2)
library(readstata13)
library(scales)
library(cowplot)


# Working directory has to be set to the directory containing the file "bechtel_liesch_scheve_pnas.dta"

# Load data
mydata <- read.dta13("bechtel_liesch_scheve_pnas.dta",
                     generate.factors = T)


# Create dataset that only includes observations with sensitivities from -2 to 2
mydata <- mydata[mydata$d_sensitivity >= -2 & mydata$d_sensitivity <= 2, ]
mydata <- mydata[mydata$a_sensitivity >= -2 & mydata$a_sensitivity <= 2, ]

################################################################################
#### Figure 2
################################################################################
pooled <- ggplot(data=mydata) + geom_histogram(aes(x=d_sensitivity,y=..count../sum(..count..), colour="Disadvantageous IA",fill="Disadvantageous IA"),binwidth =  .05) + 
  geom_histogram(aes(x=a_sensitivity,y=..count../sum(..count..), colour="Advantageous IA", fill="Advantageous IA"), alpha=.00001, binwidth = .05)  + theme_bw() + 
  ylab("Percent") + xlab("Sensitivity (Estimated Parameter)") +
  scale_colour_manual(name = "", values=c("Disadvantageous IA"="transparent", "Advantageous IA"="black")) +
  scale_fill_manual(name="", values=c("Disadvantageous IA"="gray57", "Advantageous IA"="transparent")) +
  theme(legend.position = c(.83, 0.8), legend.background = element_rect(fill="transparent"),      
        legend.key = element_blank(),   legend.key.size=unit(.5,"cm")) + 
  scale_y_continuous(labels = percent, breaks=seq(0,.25,.05), limits = c(0, .25) ) + 
  scale_x_continuous(limits = c(-2.1, 2.1)) +
  theme(legend.text=element_text(size=16)) + theme(axis.title=element_text(size=16))
pooled



# DI, by country

# Extract data for both countries
US_sensitivity <- mydata[mydata$GE == 0, ]
GE_sensitivity <- mydata[mydata$GE == 1, ]

US_sensitivity$country <- "United States"
GE_sensitivity$country <- "Germany"

# Only from -2 to 3 where most observations lie
d.sens <- ggplot() + geom_histogram(data = US_sensitivity, aes(x=d_sensitivity,y=..count../sum(..count..), colour = "United States", fill = "United States"), binwidth =  .05) + theme_bw() +
  geom_histogram(data = GE_sensitivity, aes(x=d_sensitivity,y=..count../sum(..count..), colour = "Germany", fill = "Germany"), alpha=.00001, binwidth =  .05) +
  ylab("Percent") + xlab("Sensitivity to Disadvantageous Inequality") +
  scale_colour_manual(name = "", values=c("United States"="transparent", "Germany"="black")) +
  scale_fill_manual(name = "", values=c("United States"="gray57", "Germany"="transparent")) +  
  theme(legend.position = c(.83, 0.8), legend.background = element_rect(fill="transparent"),      
        legend.key = element_blank(),   legend.key.size=unit(.5,"cm")) + 
  scale_y_continuous(labels = percent, breaks=seq(0,.25,.05), limits = c(0, .25) ) + 
  scale_x_continuous(limits = c(-2.1, 2.1)) +
  theme(legend.text=element_text(size=16)) + theme(axis.title=element_text(size=16))


# AI, by country

# Only from -2 to 3 where most observations lie
a.sens <- ggplot() + geom_histogram(data = US_sensitivity, aes(x=a_sensitivity,y=..count../sum(..count..), colour = "United States", fill = "United States"), binwidth =  .05) + theme_bw() +
  geom_histogram(data = GE_sensitivity, aes(x=a_sensitivity,y=..count../sum(..count..), colour = "Germany", fill = "Germany"), alpha=.00001, binwidth =  .05) +
  ylab("Percent") + xlab("Sensitivity to Advantageous Inequality") +
  scale_colour_manual(name = "", values=c("United States"="transparent", "Germany"="black")) +
  scale_fill_manual(name = "", values=c("United States"="gray57", "Germany"="transparent")) +  
  theme(legend.position = c(.83, 0.8), legend.background = element_rect(fill="transparent"),      
        legend.key = element_blank(),   legend.key.size=unit(.5,"cm")) + 
  scale_y_continuous(labels = percent, breaks=seq(0,.25,.05), limits = c(0, .25) ) + 
  scale_x_continuous(limits = c(-2.1, 2.1)) +
  theme(legend.text=element_text(size=16)) + theme(axis.title=element_text(size=16))


require(cowplot)
m2 <- plot_grid(pooled, d.sens, a.sens, labels = c('A','B', 'C'), ncol=1, hjust = -1, vjust=2, label_size = 14)


ggsave("Figure 2.pdf", m2, width=15, height=7)


# Working directory has to be set to the directory containing the file "bechtel_liesch_scheve_pnas.dta"

# Load data
mydata <- read.dta13("bechtel_liesch_scheve_pnas.dta",
                     generate.factors = T)


# Create dataset that only includes observations with sensitivities from -2 to 2
mydata <- mydata[mydata$d_sensitivity >= -2 & mydata$d_sensitivity <= 2, ]
mydata <- mydata[mydata$a_sensitivity >= -2 & mydata$a_sensitivity <= 2, ]

################################################################################
#### Figure 2
################################################################################
pooled <- ggplot(data=mydata) + geom_histogram(aes(x=d_sensitivity,y=..count../sum(..count..), colour="Disadvantageous IA",fill="Disadvantageous IA"),binwidth =  .05) + 
  geom_histogram(aes(x=a_sensitivity,y=..count../sum(..count..), colour="Advantageous IA", fill="Advantageous IA"), alpha=.00001, binwidth = .05)  + theme_bw() + 
  ylab("Percent") + xlab("Sensitivity (Estimated Parameter)") +
  scale_colour_manual(name = "", values=c("Disadvantageous IA"="transparent", "Advantageous IA"="black")) +
  scale_fill_manual(name="", values=c("Disadvantageous IA"="gray57", "Advantageous IA"="transparent")) +
  theme(legend.position = c(.83, 0.8), legend.background = element_rect(fill="transparent"),      
        legend.key = element_blank(),   legend.key.size=unit(.5,"cm")) + 
  scale_y_continuous(labels = percent, breaks=seq(0,.25,.05), limits = c(0, .25) ) + 
  scale_x_continuous(limits = c(-2.1, 2.1)) +
  theme(legend.text=element_text(size=16)) + theme(axis.title=element_text(size=16))
pooled



# DI, by country

# Extract data for both countries
US_sensitivity <- mydata[mydata$GE == 0, ]
GE_sensitivity <- mydata[mydata$GE == 1, ]

US_sensitivity$country <- "United States"
GE_sensitivity$country <- "Germany"

# Only from -2 to 3 where most observations lie
d.sens <- ggplot() + geom_histogram(data = US_sensitivity, aes(x=d_sensitivity,y=..count../sum(..count..), colour = "United States", fill = "United States"), binwidth =  .05) + theme_bw() +
  geom_histogram(data = GE_sensitivity, aes(x=d_sensitivity,y=..count../sum(..count..), colour = "Germany", fill = "Germany"), alpha=.00001, binwidth =  .05) +
  ylab("Percent") + xlab("Sensitivity to Disadvantageous Inequality") +
  scale_colour_manual(name = "", values=c("United States"="transparent", "Germany"="black")) +
  scale_fill_manual(name = "", values=c("United States"="gray57", "Germany"="transparent")) +  
  theme(legend.position = c(.83, 0.8), legend.background = element_rect(fill="transparent"),      
        legend.key = element_blank(),   legend.key.size=unit(.5,"cm")) + 
  scale_y_continuous(labels = percent, breaks=seq(0,.25,.05), limits = c(0, .25) ) + 
  scale_x_continuous(limits = c(-2.1, 2.1)) +
  theme(legend.text=element_text(size=16)) + theme(axis.title=element_text(size=16))


# AI, by country

# Only from -2 to 3 where most observations lie
a.sens <- ggplot() + geom_histogram(data = US_sensitivity, aes(x=a_sensitivity,y=..count../sum(..count..), colour = "United States", fill = "United States"), binwidth =  .05) + theme_bw() +
  geom_histogram(data = GE_sensitivity, aes(x=a_sensitivity,y=..count../sum(..count..), colour = "Germany", fill = "Germany"), alpha=.00001, binwidth =  .05) +
  ylab("Percent") + xlab("Sensitivity to Advantageous Inequality") +
  scale_colour_manual(name = "", values=c("United States"="transparent", "Germany"="black")) +
  scale_fill_manual(name = "", values=c("United States"="gray57", "Germany"="transparent")) +  
  theme(legend.position = c(.83, 0.8), legend.background = element_rect(fill="transparent"),      
        legend.key = element_blank(),   legend.key.size=unit(.5,"cm")) + 
  scale_y_continuous(labels = percent, breaks=seq(0,.25,.05), limits = c(0, .25) ) + 
  scale_x_continuous(limits = c(-2.1, 2.1)) +
  theme(legend.text=element_text(size=16)) + theme(axis.title=element_text(size=16))


require(cowplot)
m2 <- plot_grid(pooled, d.sens, a.sens, labels = c('A','B', 'C'), ncol=1, hjust = -1, vjust=2, label_size = 14)


ggsave("Figure 2.pdf", m2, width=15, height=7)

