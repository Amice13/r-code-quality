#***********************************************************************#
#Replication code for Quality Test of 
#“To substantially reduce bribery and corruption in all its forms”.
#A Multidimensional Index of Progress on Sustainable Development Goal 16.5
#***********************************************************************#

#Required Packages
install.packages("dplyr")  
install.packages("ggplot2") 

library(dplyr) 
library(ggplot2)

##Charge data
dta <- read.csv("MIPAC_quality_test.csv", header=T,stringsAsFactors=FALSE,sep=",")

# Filter out countries with mean_mipac = NaN
dta <- dta %>%
  filter(!is.na(mipac))

# Create the plot
uncer <- ggplot(dta %>% arrange(desc(mipac)) %>%
                  mutate(country = factor(country, levels = country))) +
  geom_point(aes(x = country, y = mipac), size = 2) +
  geom_point(aes(x = country, y = mipac25), shape = 4) +
  geom_errorbar(aes(ymin = mipac50, ymax = mipac10, y = mipac, x = country)) +
  labs(x = "Countries", y = "MIPAC with different Thresholds") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 7))
uncer