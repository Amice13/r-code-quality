################################################################################
# Immigrants, elections, and turnout
# Author: Linuz Aggeborn, Henrik Andersson, Sirus Dehdari, Karl-Oskar Lindgren
# Description: Create Figure 1 in main paper
################################################################################


# We save paths for input and output:
out_img <- "..//figures//" 


# Loading packages
library(ggplot2)


### Create a table with data (see paper for source for each country):
df_tab <- data.frame(Country = rep(c("Sweden", "Norway", "Denmark", "Finland"),3), 
                     Turnout = c(90,70,75,59,73,45,55,36,38,31,32,21), 
                     Voters = c(rep("Natives",4), rep("Naturalized immigrants",4),
                       rep("Non-naturalized immigrants",4)))



## Generating the graph with ggplot:
localvoting_ggplot <-ggplot(df_tab, aes(x = Voters, y = Turnout, group = Country))

pdf(paste(out_img,"scandinavia_turnout.pdf", sep = "")) 
localvoting_ggplot + 
  geom_point(aes(shape = Country), size= 3,color = "black") + 
  geom_line(aes(linetype = Country), size =0.5,color = "black") + 
  theme_bw(base_size = 14) + 
  labs(y= "Turnout in local elections",x = "Voter group")  + 
  scale_shape_manual(values = 15:18) + 
  theme(legend.position="bottom",legend.title = element_text(colour="black", size=16, face="bold")) 
dev.off()






















