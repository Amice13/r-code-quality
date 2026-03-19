require(ggplot2)

#setwd("")

df <- read.csv("rsvb_Jan2003_Aug2017.csv", header = TRUE, sep = ",")
monYrOrder <- read.table("monYr_order.txt", header = TRUE)
head(df)
str(df)
head(monYrOrder)
str(factor(monYrOrder))

pdf("RSVB_epidemics_5Jul.pdf")

ggplot(df, aes(x = factor(monYr, levels = monYrOrder$monYr), y = rsvb_bin, group = 1)) + 
  geom_area(color = "#00AFBB", fill = "#00AFBB",alpha = 0.5, position = position_dodge(0.8)) +
  geom_line(aes(x= factor(monYr, levels = monYrOrder$monYr), y = rsva_bin, group = 1), 
            color = "orange", size = 0.5) +
  geom_segment(aes(x = 126, y = 67, xend = 130, yend = 67), color = "orange") +
  geom_text(aes(x = 140, y = 67, label = "RSVA")) +
  geom_segment(aes(x = 126, y = 60, xend = 130, yend = 60), color = "#00AFBB") +
  geom_text(aes(x = 140, y = 60, label = "RSVB")) +
  theme_classic() +
  labs(x = "", y = "RSV cases\n") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_discrete(breaks = c("Jan-03","Oct-03","Sep-04","Oct-05","Sep-06","Oct-07","Sep-08","Oct-09","Sep-10","Oct-11","Sep-12","Oct-13","Sep-14","Oct-15","Sep-16","Jul-17"))

dev.off()  


