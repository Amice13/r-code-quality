# A2_World_Value_Survey.R
# Purpose: The Role of Public Broadcasting in Media Bias
# Created: 2021-6-11 Taka-aki Asano
# Last Modified: 2021-10-14

# package
require("readr")
require("dplyr")
require("tidyr")
require("ggplot2")
theme_set(theme_classic(base_size = 12))


# dataset
gov <- read_csv(
  "WVS_Confidence_The_Government_excluding_DKNA_count.csv", 
  locale = locale(encoding = "UTF8")
)
tv <- read_csv(
  "WVS_Confidence_Television_excluding_DKNA_count.csv", 
  locale = locale(encoding = "UTF8")
)


# rate
gov[,2] <- gov[,2] / gov[,6]
gov[,3] <- gov[,3] / gov[,6]
gov[,4] <- gov[,4] / gov[,6]
gov[,5] <- gov[,5] / gov[,6]
tv[,2] <- tv[,2] / tv[,6]
tv[,3] <- tv[,3] / tv[,6]
tv[,4] <- tv[,4] / tv[,6]
tv[,5] <- tv[,5] / tv[,6]


# mean
gov$Mean_Gov <- 4 * gov$`A great deal` + 3 * gov$`Quite a lot` + 
  2 * gov$`Not very much` + 1 * gov$`None at all`
tv$Mean_TV <- 4 * tv$`A great deal` + 3 * tv$`Quite a lot` + 
  2 * tv$`Not very much` + 1 * tv$`None at all`


# Figure A4
confidence <- left_join(
  gov[,c("Country", "Mean_Gov")], tv[,c("Country", "Mean_TV")], by = "Country"
)
confidence$Label <- confidence$Country
confidence$Label[!(confidence$Country %in% c("Japan", "United States", "China", "Germany"))] <- NA
confidence2 <- confidence[!is.na(confidence$Label),]
confidence_plot <- ggplot(confidence, aes(x = Mean_Gov, y = Mean_TV)) + 
  geom_point() + xlim(1.5, 3.5) + ylim(1.5, 3.5) + 
  geom_label_repel(data = confidence, aes(label = Label)) +
  geom_point(data = confidence2, size = 5, shape = 1) + 
  labs(x = expression("None at all" %<-% "Confidence: Government" %->% "A great deal"), 
       y = expression("None at all" %<-% "Confidence: Television" %->% "A great deal"), 
       title = "World Value Survey Wave 7: 2017-2020", 
       caption = "Source: https://www.worldvaluessurvey.org/WVSOnline.jsp") + 
  theme(plot.title = element_text(hjust = 0.5))
plot(confidence_plot)
