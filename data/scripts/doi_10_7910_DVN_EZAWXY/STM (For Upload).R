library(data.table)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(ggplot2)
library(stm)
library(arrow)
library(stringr)
library(slam)
library(plyr)
library(lubridate)
library(dplyr)
library(readr)
library(reshape2)
library(scales)

options(ggrepel.max.overlaps = Inf)


#Load and preprocess dataset
load("abortion_beforeQuanteda.RData")
result_key <- result_key[,!(names(result_key) %in% c("Period", "Content", "Content_WithIndex_trim"))]

result_key[result_key$Press == "Fox News", ]$Press <- "Fox"
result_key$TwoPeriod_Press <- NA
result_key$TwoPeriod_Press[result_key$Press == "Fox" & as_date(result_key$Date) < as_date("2017-04-10")] <- "bef.2017_Fox"
result_key$TwoPeriod_Press[result_key$Press == "Fox" & as_date(result_key$Date) >= as_date("2017-04-10")] <- "aft.2017_Fox"
result_key$TwoPeriod_Press[result_key$Press == "MSNBC" & as_date(result_key$Date) < as_date("2017-04-10")] <- "bef.2017_MSNBC"
result_key$TwoPeriod_Press[result_key$Press == "MSNBC" & as_date(result_key$Date) >= as_date("2017-04-10")] <- "aft.2017_MSNBC"

#Create dfm using Quanteda
result_key$Content_Untag <- gsub("([^\\(]+)(\\([^\\,]+,)([0-9]+\\))", "\\1", result_key$Content_clean)
corpus <- corpus(result_key, text_field = "Content_Untag")
toks <- tokens_split(tokens(corpus, what = "fastestword", remove_punct = FALSE), separator = " ", remove_separator = TRUE)
dfmat_key <- dfm(toks)
save(result_key, dfmat_key, file = "ForLDA.RData")

# Check vocabulary
write.csv(textstat_frequency(dfmat_key), "WordFre_ForLDA.csv", row.names=FALSE)

#STM Topic Modeling
mydfm_stm <- convert(dfmat_key, to = "stm")
out <- prepDocuments(mydfm_stm$documents, mydfm_stm$vocab, mydfm_stm$meta, lower.thresh = 5)
out$meta$Year <- as.numeric(out$meta$Year)

set.seed(900110)
model <- stm(out$documents, out$vocab, K = 50, prevalence = ~Press + s(Year), max.em.its = 75, data = out$meta, init.type = "Spectral")

#Extract topic words
label.topic <- capture.output(labelTopics(model, n = 50))
label.topic <- gsub("\\t Lift.+", "\n", label.topic)
label.topic <- gsub("\\t Score.+", "\n", label.topic)
writeLines(label.topic, "topic_words(50_All).txt")

#Topic distribution by media
topic_media <- rbind(
  Fox = col_means(model$theta[out$meta$Press == "Fox",]),
  MSNBC = col_means(model$theta[out$meta$Press == "MSNBC",])
)
write.csv(topic_media, "topic_distribution_t50_bymedia.csv")

#Topic distribution by Year
topic_year <- aggregate(model$theta, by = list(Year = out$meta$Year), FUN = mean)
write.csv(topic_year, "Topic_Distribution_Year_All.csv")

#Topic distribution by Period-Media
Topic_labels <- read_csv("Topic_Label_Interpret_t50.csv")
topic_period_media <- aggregate(model$theta, by = list(Period_Media = out$meta$TwoPeriod_Press), FUN = mean)
colnames(topic_period_media)[-1] <- Topic_labels$Cate_sub
topic_period_media$Period_Media <- gsub("([0-9]+-[0-9]+)(_)([a-zA-Z]+)", "\\3(\\1)", topic_period_media$Period_Media)
write.csv(topic_period_media, "Topic_Distribution_Period-Press_All.csv")

#Plot topic distribution (Period-Media)
topic_melt <- melt(topic_period_media, id = "Period_Media")
tiff("Topic_Distribution_Period-Media_All.tiff", width = 10, height = 6, units = 'in', res = 200)
ggplot(topic_melt, aes(x=Period_Media, y=value, fill=variable, label=percent(value))) +
  geom_bar(stat="identity") +
  geom_text(size=3, position=position_stack(vjust=0.5)) +
  labs(title="Topic Distribution by Period and Media", x="", y="Share [%]")
dev.off()

#Detailed category analysis
for(cat in unique(Topic_labels$Cate)){
  subset_labels <- Topic_labels$Cate_sub[Topic_labels$Cate == cat]
  subset_data <- topic_period_media[,c("Period_Media", subset_labels)]
  write.csv(subset_data, paste0("Topic_detail_All_", cat, ".csv"), row.names=FALSE)
  
  subset_melt <- melt(subset_data, id="Period_Media")
  tiff(paste0("Topic_Detail_Distribution_", cat, ".tiff"), width=11, height=6, res=200)
  print(ggplot(subset_melt, aes(x=Period_Media, y=value, fill=variable, label=percent(value))) +
          geom_bar(stat="identity") +
          geom_text(size=3, position=position_stack(vjust=0.5)) +
          labs(title=paste("Detailed Topic Distribution -", cat), x="", y="Share [%]"))
  dev.off()
}
