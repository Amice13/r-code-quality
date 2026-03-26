sink(file="figure5_log.txt")

#########################
# Code to create Figure 5 in 
# ``Democracy by Deterrence: Norms, Constitutions, and Electoral Tilting"
#########################

# Install libraries
install.packages("ggplot2")
install.packages("farver")
install.packages("gridExtra")


# Load libraries

library(ggplot2)
library(farver)
library(gridExtra)

# Set the working directory
#setwd("")


# Load data
load("tscs.RData")


####
# Figures

# Change WD to where you want the figures to output
#setwd("")

####

y <- aggregate(summary_vote ~ half_decade + trifecta, v, mean)


p2 <- ggplot(y, aes(x = half_decade, y = summary_vote, group=trifecta, linetype=trifecta, color=trifecta)) +
  geom_line() +
  geom_point() +
  scale_linetype_manual(values=c('solid','dotted','dashed'), labels=c("Democrat", "Divided", "Republican")) +
  scale_color_manual(values=c('Blue','purple1','Red'), labels=c("Democrat", "Divided", "Republican")) +
  labs(title = "A. Aggregate Voting Restrictions (0-3)",
       x = "Time Period",
       y = "Average") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1), axis.title=element_text(size=8), plot.title = element_text(size = 12)) +
  scale_y_continuous(limits=c(0,3)) +
  theme(legend.position="none") +
  theme(axis.title.x = element_blank())



####

y <- aggregate(no_absentee ~ half_decade + trifecta, v, mean)


p4 <- ggplot(y, aes(x = half_decade, y = no_absentee, group=trifecta, linetype=trifecta, color=trifecta)) +
  geom_line() +
  geom_point() +
  scale_linetype_manual(values=c('solid','dotted','dashed'), labels=c("Democrat", "Divided", "Republican")) +
  scale_color_manual(values=c('Blue','purple1','Red'), labels=c("Democrat", "Divided", "Republican")) +
  labs(title = "D. Restrictions on Absentee Voting",
       x = "",
       y = "Fraction") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1), axis.title=element_text(size=8), plot.title = element_text(size = 12)) +
  scale_y_continuous(limits=c(0,1)) +
  theme(legend.position="none") +   theme(axis.title.x = element_blank())


####

y <- aggregate(strictPhotoID ~ half_decade + trifecta, v, mean)


p5 <- ggplot(y, aes(x = half_decade, y = strictPhotoID, group=trifecta, linetype=trifecta, color=trifecta)) +
  geom_line() +
  geom_point() +
  scale_linetype_manual(values=c('solid','dotted','dashed'), labels=c("Democrat", "Divided", "Republican")) +
  scale_color_manual(values=c('Blue','purple1','Red'), labels=c("Democrat", "Divided", "Republican")) +
  labs(title = "B. Require Photo Voter ID",
       x = "",
       y = "Fraction") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1), axis.title=element_text(size=8), plot.title = element_text(size = 12)) +
  scale_y_continuous(limits=c(0,1)) +
  theme(legend.position="none") +   theme(axis.title.x = element_blank())



####

y <- aggregate(binary_exfelon_disenfran ~ half_decade + trifecta, v, mean)


p6 <- ggplot(y, aes(x = half_decade, y = binary_exfelon_disenfran, group=trifecta, linetype=trifecta, color=trifecta)) +
  geom_line() +
  geom_point() +
  scale_linetype_manual(values=c('solid','dotted','dashed'), labels=c("Democrat", "Divided", "Republican")) +
  scale_color_manual(values=c('Blue','purple1','Red'), labels=c("Democrat", "Divided", "Republican")) +
  labs(title = "C. Felons Permanently Ineligible",
       x = "",
       y = "Fraction") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1), axis.title=element_text(size=8), plot.title = element_text(size = 12)) +
  scale_y_continuous(limits=c(0,1)) +
  theme(legend.position="none") +   theme(axis.title.x = element_blank())



########
# Combine into one plot
# With a common legend

p <- grid.arrange(arrangeGrob(p2, p5, p6, p4))

p
ggsave("comboFig_ed.pdf", p, height=6.99, width=6.99)
