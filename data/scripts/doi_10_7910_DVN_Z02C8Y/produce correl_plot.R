library(ggplot2)
data <- read.csv("C:/Users/gschuma1/Dropbox/CHES/Papers/Anti_Elite_Corruption_Paper/Data/_R&R_Oct2016/populism_text_analyis.csv", sep=";")
colnames(data) <- c("party", "year", "text.populism", "survey.populism")
cor(data$text.populism, data$survey.populism, use="pairwise.complete.obs")
cor(data$text.populism[-c(1,12,19)], data$survey.populism[-c(1,12,19)], use="pairwise.complete.obs")

corplot <- ggplot(data, aes(x=survey.populism, y=text.populism, label=party)) +
  geom_text(size=2) +
  xlab("Salience of anti-elite rhetoric") +
  ylab("Populism in manifesto") +
  theme_minimal() +
  annotate("text", x=9.5, y=.175, label="r =.51") 
corplot

ggsave(corplot, file="correl_plot.jpg")
