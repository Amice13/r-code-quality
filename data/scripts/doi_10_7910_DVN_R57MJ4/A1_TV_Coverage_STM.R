# A1_TV_Coverage_STM.R
# Purpose: Role of Public Media in Rhetoric and Propaganda
# Created: 2018-12-9 Taka-aki Asano
# Last Modified: 2021-10-14

# package
require("stringr")
require("dplyr")
require("tidyr")
require("stm")
require("vegan")
require("ggplot2")
require("Hmisc")


# load document-feature matrix
load("TV_Coverage_DFM.Rdata")


# specify the number of topic
numbers <- c(seq(10, 100, by = 10), seq(120, 200, by = 20))
exclusivity <- data.frame(Numbers = numbers, 
                          Exclusivity = rep(NA, length(numbers)))
for(i in numbers) {
  print(Sys.time())
  set.seed(2018)
  res <- stm(
    documents = coverage.stm$documents, 
    vocab = coverage.stm$vocab, 
    K = i, 
    data = coverage.stm$meta
  )
  exclusivity$Exclusivity[exclusivity$Numbers == i] <- mean(exclusivity(res))
}
## Figure A.2
exclusivity.plot <- ggplot(exclusivity, aes(x = Numbers, y = Exclusivity)) + 
  geom_point() + geom_line() + 
  labs(x = "Number of Topics", y = "Average Exclusivity") + 
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(hjust = 0, color = "black"), 
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        plot.title = element_blank(), 
        panel.background = element_rect(fill = "white", color = "black"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(linetype = 3, color = "darkgray"), 
        axis.ticks.y = element_blank())
plot(exclusivity.plot)


# structural topic model
set.seed(2018)
stm.coverage <- stm(
  documents = coverage.stm$documents, 
  vocab = coverage.stm$vocab, 
  K = 100, 
  data = coverage.stm$meta
)


# save
save(list = c("exclusivity", "stm.coverage"), file = "TV_Coverage_STM.Rdata")
## in loading .Rdata
## load("TV_Coverage_STM.Rdata")

# top words by topic
(word.stm.coverage <- labelTopics(stm.coverage, n = 10))


# correlations between topics
cor.topics <- topicCorr(stm.coverage)


# specify topic of each program
topic.coverage <- apply(stm.coverage$theta, 1, function(x) which(x == max(x)))
coverage.stm$meta <- mutate(coverage.stm$meta, Topic = topic.coverage)
coverage.data <- coverage.stm$meta


# variety of topics
variety <- aggregate(
  Topic ~ Year + Station, 
  coverage.data, 
  function(i){return(length(unique(i)))}
)
## Figure A.3
variety.plot <- ggplot(variety, aes(x = Station, y = Topic, group = Year)) + 
  geom_col() + facet_wrap(~Year, ncol = 3, scales =  "free") + 
  labs(x = "", y = "Number of Topics", title = "") + 
  theme(axis.text.x = element_text(size = 12), 
        axis.title.x = element_blank(),
        plot.title = element_blank())
plot(variety.plot)


# mean of number of topics
aggregate(Topic ~ Station, variety, mean)
mean(variety$Topic[variety$Station != "NHK"])
t.test(variety$Topic[variety$Station != "NHK"], 
       variety$Topic[variety$Station == "NHK"])
