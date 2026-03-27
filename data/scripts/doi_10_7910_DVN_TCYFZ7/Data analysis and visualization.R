library(tidyverse)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(ggradar2)
library(scales)
library(circlize)
library(ggraph)
library(igraph)
library(RColorBrewer)
library(JustifyAlpha)
library(tibble)
library(patchwork)
library(scales) 
library(openxlsx)
library(readxl)


setwd("") # fill in the path of your working directory, and put all the downloaded data files in your working directory

df <- read_csv("df7_3EmotionsCH_region_singerType_Year2.csv")
df$region_coarse[is.na(df$region_coarse)] <- 'unknown'
df$region[is.na(df$region)] <- 'unknown'

dF <- read_csv('df7_3EmotionsCH_split_synonym_expanded_emo scores_final.csv')
dF <- dF %>% filter(Year2 >= 1967) %>%
  filter(IntensityWithSign <= 15, IntensityWithSign >= -15)
dF$region_coarse[is.na(dF$region_coarse)] <- 'unknown'
dF$region[is.na(dF$region)] <- 'unknown'

Df <- read_csv("df7_Ch_Split_region_singerType_Year2_per_100Chars.csv")
Df$region_coarse[is.na(Df$region_coarse)] <- 'unknown'
Df$region[is.na(Df$region)] <- 'unknown'

DF <- read_csv('df7_ch_split_emo scores_final_per_100Chars.csv')
DF$region_coarse[is.na(DF$region_coarse)] <- 'unknown'
DF$region[is.na(DF$region)] <- 'unknown'
DF <- DF %>% filter(Year2 >= 1967)

dF <- dF %>% semi_join(DF, by = c("id" = "id"))
DF <- DF %>% semi_join(dF, by = c("id" = "id")) 


# 1. Time trends ----

## 1.0.1 log(# of songs) over year----
p <- DF %>% group_by(Year2) %>%
  summarise(ct = n()) %>%
  ggplot(aes(x = Year2, y = log10(ct))) +
  geom_line(size=1, colour="black") +
  # geom_point(size=1, colour="black") +
  # geom_smooth(method = "lm", size = 0.5, colour = "black") +
  labs(x = "Year", y = "log(# of songs)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  scale_y_continuous(breaks = seq(0, 11, by = 1)) +
  expand_limits(y = 0)+
  theme(
    panel.background = element_rect(fill = "white", colour = NA), # Make panel background white
    plot.background = element_rect(fill = "white", colour = NA)   # Make overall plot background white
  )

p

ggsave("Fig 12.tiff", plot = p, width = 6, height = 4, dpi = 300, units = "in", compression = "lzw")

## 1.0.2 singerType percentages over year----

df1 <- dF %>% select(Year2, singerType, region_coarse)

df2 <- df1 %>% group_by(Year2, singerType) %>%
  summarise(ct = n(), log_ct = log(n())) %>%
  mutate(percentage = ct / sum(ct))

df0 <- df1 %>% group_by(Year2, singerType) %>%
  summarise(ct = n()) %>%
  summarise(ct = n())

tp <- data.frame(
  Year2 = c(1967, 1969, 1971, 1972),
  singerType = rep("group", 4),
  ct = rep(0, 4),
  log_ct = rep(NA, 4),
  percentage = rep(0,4)
)

df2 <- rbind(df2, tp)
df2$singerType <- factor(df2$singerType)
df2$singerType <- fct_relevel(df2$singerType, 'group', 'male', 'female' )

gray_shades <- c(
  "female" = "gray80",  # Light gray
  "male" = "gray45",  # Medium-dark gray
  "group" = "gray10"   # Dark gray
)

p <- df2 %>% ggplot(aes(x = Year2, y = percentage, fill = singerType)) +
  geom_area(alpha = 0.6, size = 0.2, colour = "white") +
  labs(x = "Year", y = "Percentages of singer types") +
  theme_minimal() +
  guides(fill = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent) +
    scale_fill_manual(values = gray_shades)+
  theme(
    panel.background = element_rect(fill = "white", colour = NA), # Make panel background white
    plot.background = element_rect(fill = "white", colour = NA)   # Make overall plot background white
  )

p

ggsave("Fig 13.tiff", plot = p, width = 6, height = 4, dpi = 300, units = "in", compression = "lzw")

## 1.0.3 Avg. lyric length over year----

p <- DF %>% 
  group_by(Year2) %>%
  summarise(ch_len = mean(ch_len)) %>%
  filter(Year2 >= 1967) %>%
  ggplot(aes(x = Year2)) +
  geom_point(aes(y = ch_len), color = "black", size = 1) +
  geom_smooth(aes(y = ch_len), color = "black", 
              size = 0.5, span = 0.5) +
  labs(x = "Year", y = "Avg. lyric length (# of Chinese characters)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )+
  theme(
    panel.background = element_rect(fill = "white", colour = NA), # Make panel background white
    plot.background = element_rect(fill = "white", colour = NA)   # Make overall plot background white
  )

p

ggsave("Fig 17.tiff", plot = p, width = 6, height = 4, dpi = 300, units = "in", compression = "lzw")


## 1.1 Overall average sentiment score trend (graphs: line, ridgeline, stacked area) ----

### 1.1.1 Three GPT-summarized emotion words ----

p1 <- dF %>% 
  group_by(Year2) %>%
  summarise(IntensityWithSign_avg = mean(IntensityWithSign)) %>%
  filter(Year2 >= 1967) %>%
  ggplot(aes(x = Year2)) +
  geom_point(aes(y = IntensityWithSign_avg), color = "black", size = 1) +
  geom_smooth(aes(y = IntensityWithSign_avg), color = "black", 
              size = 0.5, span = 0.5) +
  labs(x = "(a)", y = "Avg. total sentiment", title = "ChatGPT + Lexicon") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

p1

### 1.1.2 Lyric split ----

p2 <- DF %>% 
  group_by(Year2) %>%
  summarise(IntensityWithSign_avg = mean(IntensityWithSign)) %>%
  filter(Year2 >= 1967) %>%
  ggplot(aes(x = Year2)) +
  geom_point(aes(y = IntensityWithSign_avg), color = "black", size = 1) +
  geom_smooth(aes(y = IntensityWithSign_avg), color = "black", 
              size = 0.5, span = 0.5) +
  labs(x = "(b)", y = NULL, title = "Lexicon only") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

p2

### 1.1.3 Combined together ----

P <- p1 + p2
P

ggsave("Fig 2.tiff", plot = P, width = 6, height = 3, dpi = 300, units = "in", compression = "lzw")

### 1.1.4 Overall sentiment trends by region  ----

gray_shades <- c(
  "Taiwan"   = "gray85",
  "China-Main" = "gray1",
  "Hong Kong"  = "gray60"
)

p1 <- dF %>% filter(region_coarse %in% c("China-Main","Hong Kong","Taiwan")) %>%
  group_by(Year2, region_coarse) %>%
  summarise(IntensityWithSign_avg = mean(IntensityWithSign), .groups="drop") %>%
  filter(between(IntensityWithSign_avg, -5, 5)) %>%
  ggplot(aes(x = Year2, y = IntensityWithSign_avg, color = region_coarse)) +
  geom_smooth(size = 1, span = 0.5, se = FALSE) +
  scale_color_manual(
    name = NULL,
    values = gray_shades
  ) +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  labs(x = "Year", y = "Avg. total sentiment") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )  +
  theme(
    panel.background = element_rect(fill = "white", colour = NA), # Make panel background white
    plot.background = element_rect(fill = "white", colour = NA)   # Make overall plot background white
  )

p1

ggsave("Fig 16.tiff", plot = p1, width = 6, height = 4, dpi = 300, units = "in", compression = "lzw")


## 1.2 Overall distribution of sentiment score trend (ridgeline) ----

### 1.2.1 Three GPT-summarized emotion words ----

tp <- dF %>%
  select(Year2, IntensityWithSign) %>%
  arrange(Year2)

tp$halfDec[tp$Year2 >= 1967 & tp$Year2 <= 1974] <- "1967-1974"
tp$halfDec[tp$Year2 >= 1975 & tp$Year2 <= 1979] <- "1975-1979"
tp$halfDec[tp$Year2 >= 1980 & tp$Year2 <= 1984] <- "1980-1984"
tp$halfDec[tp$Year2 >= 1985 & tp$Year2 <= 1989] <- "1985-1989"
tp$halfDec[tp$Year2 >= 1990 & tp$Year2 <= 1994] <- "1990-1994"
tp$halfDec[tp$Year2 >= 1995 & tp$Year2 <= 1999] <- "1995-1999"
tp$halfDec[tp$Year2 >= 2000 & tp$Year2 <= 2004] <- "2000-2004"
tp$halfDec[tp$Year2 >= 2005 & tp$Year2 <= 2009] <- "2005-2009"
tp$halfDec[tp$Year2 >= 2010 & tp$Year2 <= 2014] <- "2010-2014"
tp$halfDec[tp$Year2 >= 2015 & tp$Year2 <= 2019] <- "2015-2019"
tp$halfDec[tp$Year2 >= 2020 & tp$Year2 <= 2023] <- "2020-2023"

tp$halfDec <- factor(tp$halfDec, levels = c(
  "1967-1974",
  "1975-1979",
  "1980-1984",
  "1985-1989",
  "1990-1994",
  "1995-1999",
  "2000-2004",
  "2005-2009",
  "2010-2014",
  "2015-2019",
  "2020-2023"
))

# tp <- tp %>% mutate(halfDec = fct_rev(halfDec)) 

p1 <- tp %>%
  ggplot(aes(x = IntensityWithSign, y = halfDec, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01) +
  scale_fill_gradient(low = "black", high = "gray100", name = "Sentiment Score") + # grayscale gradient
  theme_minimal() +
  labs(x = "Lyric sentiment\n(a)", y = "Half decades", title = "ChatGPT + Lexicon") +
  theme(legend.position = "none",
        panel.spacing = unit(0.5, "lines"),
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(-15, 15, by = 5), limits = c(-15, 15))

p1

### 1.2.2 Lyric split ----

tp1 <- DF %>%
  select(Year2, IntensityWithSign) %>%
  filter(IntensityWithSign <= 50, IntensityWithSign >= -50) %>%
  arrange(Year2)


tp1$halfDec[tp1$Year2 >= 1967 & tp1$Year2 <= 1974] <- "1967-1974"
tp1$halfDec[tp1$Year2 >= 1975 & tp1$Year2 <= 1979] <- "1975-1979"
tp1$halfDec[tp1$Year2 >= 1980 & tp1$Year2 <= 1984] <- "1980-1984"
tp1$halfDec[tp1$Year2 >= 1985 & tp1$Year2 <= 1989] <- "1985-1989"
tp1$halfDec[tp1$Year2 >= 1990 & tp1$Year2 <= 1994] <- "1990-1994"
tp1$halfDec[tp1$Year2 >= 1995 & tp1$Year2 <= 1999] <- "1995-1999"
tp1$halfDec[tp1$Year2 >= 2000 & tp1$Year2 <= 2004] <- "2000-2004"
tp1$halfDec[tp1$Year2 >= 2005 & tp1$Year2 <= 2009] <- "2005-2009"
tp1$halfDec[tp1$Year2 >= 2010 & tp1$Year2 <= 2014] <- "2010-2014"
tp1$halfDec[tp1$Year2 >= 2015 & tp1$Year2 <= 2019] <- "2015-2019"
tp1$halfDec[tp1$Year2 >= 2020 & tp1$Year2 <= 2023] <- "2020-2023"

tp1$halfDec <- factor(tp1$halfDec, levels = c(
  "1967-1974",
  "1975-1979",
  "1980-1984",
  "1985-1989",
  "1990-1994",
  "1995-1999",
  "2000-2004",
  "2005-2009",
  "2010-2014",
  "2015-2019",
  "2020-2023"
))

p2 <- tp1 %>%
  ggplot(aes(x = IntensityWithSign, y = halfDec, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 4, rel_min_height = 0.01) +
  scale_fill_gradient(low = "black", high = "gray100", name = "Sentiment Score") + # grayscale gradient
  theme_minimal() +
  labs(x = "Lyric sentiment\n(b)", y = NULL, title = "Lexicon Only") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_blank()) +
  scale_x_continuous(breaks = seq(-40, 50, by = 10), limits = c(-40, 50))

p2

### 1.2.3 Combined together ----

P <- p1 + p2
P

ggsave("Fig 3.tiff", plot = P, width = 6, height = 3, dpi = 300, units = "in", compression = "lzw")


### 1.2.4 Three GPT words: distribution of sentiment scores by singerType ----

tp <- dF %>%
  select(singerType, IntensityWithSign) %>%
  arrange(singerType) %>%
  mutate(singerType = factor(singerType, levels = c("female", "male", "group")))

p1 <- tp %>%
  ggplot(aes(x = IntensityWithSign, y = singerType, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_gradient(low = "black", high = "gray100", name = "Sentiment Score") + # grayscale gradient
  theme_minimal() +
  labs(x = "Lyric sentiment\n(a)", y = "Singer type", title = "ChatGPT + Lexicon") +
  theme(legend.position = "none",
        panel.spacing = unit(0.5, "lines"),
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(-15, 15, by = 5), limits = c(-15, 15))

p1

### 1.2.5 Lyric split: distribution of sentiment scores by singerType ----

tp1 <- DF %>%
  select(singerType, IntensityWithSign) %>%
  arrange(singerType) %>%
  mutate(singerType = factor(singerType, levels = c("female", "male", "group")))

p2 <- tp1 %>%
  ggplot(aes(x = IntensityWithSign, y = singerType, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_gradient(low = "black", high = "gray100", name = "Sentiment Score") + # grayscale gradient
  theme_minimal() +
  labs(x = "Lyric sentiment\n(b)", y = NULL, title = "Lexicon Only") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_blank()) +
  scale_x_continuous(breaks = seq(-40, 50, by = 10), limits = c(-40, 50))

p2

### 1.2.6 Combined together ----

P <- p1 + p2
P

ggsave("Fig 14.tiff", plot = P, width = 6, height = 3, dpi = 300, units = "in", compression = "lzw")


## 1.3 Trends of positive and negative sentiments (line) ----

### 1.3.2 Avg. positive vs. negative intensity ----

#### 1.3.2.1 Three GPT words-----

df1 <- df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(pos_intensity_gpt = weighted.mean(pos_Intensity_avg, count))

df2 <- df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(neg_intensity_gpt = weighted.mean(neg_Intensity_avg, count)) %>%
  select(neg_intensity_gpt)

tp <- cbind(df1, df2)

p1 <- tp %>% ggplot(aes(x = Year2)) +
  geom_point(aes(y = pos_intensity_gpt), color = "black", size = 1) +
  geom_smooth(aes(y = pos_intensity_gpt), color = 'black', span = 0.5, size = 0.5) +
  geom_point(aes(y = neg_intensity_gpt), color = "black", size =1, shape = 1) +
  geom_smooth(aes(y = neg_intensity_gpt), color = 'black', span = 0.5, linetype = 'dashed', size = 0.5)+
  labs(x = "(a)", y = "Positive (dots) vs. Negative (circles)\navg. sentiment intensity", title = "ChatGPT + Lexicon")+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
  )+
  guides(color = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

p1

#### 1.3.2.2 Lyric split-----

Df1 <- Df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(pos_intensity_split = weighted.mean(pos_Intensity_avg, count))

Df2 <- Df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(neg_intensity_split = weighted.mean(neg_Intensity_avg, count)) %>%
  select(neg_intensity_split)

tp <- cbind(Df1, Df2)

p2 <- tp %>% ggplot(aes(x = Year2)) +
  geom_point(aes(y = pos_intensity_split), color = "black", size = 1) +
  geom_smooth(aes(y = pos_intensity_split), color = 'black', span = 0.5, size = 0.5) +
  geom_point(aes(y = neg_intensity_split), color = "black", size =1, shape = 1) +
  geom_smooth(aes(y = neg_intensity_split), color = 'black', span = 0.5, linetype = 'dashed', size = 0.5)+
  labs(x = "(b)", y = NULL, title = "Lexicon Only")+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
  )+
  guides(color = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

p2

#### 1.3.2.3 Combined together-----

P <- p1 + p2
P

ggsave("Fig 4.tiff", plot = P, width = 6, height = 3, dpi = 300, units = "in", compression = "lzw")

## 1.4 Trends of eight emotions ----

### 1.4.1 Joy and Sadness ----

#### 1.4.1.2 Intensity ----

###### a) Three GPT words----

df1 <- df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(joy_Intensity_gpt = weighted.mean(joy_Intensity_avg, count))

df2 <- df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(sadness_Intensity_gpt = weighted.mean(sadness_Intensity_avg, count)) %>%
  select(sadness_Intensity_gpt)

tp <- cbind(df1, df2)

p1 <- tp %>% ggplot(aes(x = Year2)) +
  geom_point(aes(y = joy_Intensity_gpt), color = "black", size = 1) +
  geom_smooth(aes(y = joy_Intensity_gpt), color = 'black', span = 0.5, size = 0.5) +
  geom_point(aes(y = sadness_Intensity_gpt), color = "black", size =1, shape = 1) +
  geom_smooth(aes(y = sadness_Intensity_gpt), color = 'black', span = 0.5, linetype = 'dashed', size = 0.5)+
  labs(x = "(a)", y = "Joy (dots) vs. Sadness (circles)\navg. intensity", title = "ChatGPT + Lexicon")+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
  )+
  guides(color = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

p1

##### b) Lyric split----

Df1 <- Df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(joy_Intensity_split = weighted.mean(joy_Intensity_avg, count))

Df2 <- Df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(sadness_Intensity_split = weighted.mean(sadness_Intensity_avg, count)) %>%
  select(sadness_Intensity_split)

tp <- cbind(Df1, Df2)

p2 <- tp %>% ggplot(aes(x = Year2)) +
  geom_point(aes(y = joy_Intensity_split), color = "black", size = 1) +
  geom_smooth(aes(y = joy_Intensity_split), color = 'black', span = 0.5, size = 0.5) +
  geom_point(aes(y = sadness_Intensity_split), color = "black", size =1, shape = 1) +
  geom_smooth(aes(y = sadness_Intensity_split), color = 'black', span = 0.5, linetype = 'dashed', size = 0.5)+
  labs(x = "(b)", y = NULL, title = "Lexicon Only")+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
  )+
  guides(color = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

p2

### 1.4.2 Liking and Disgust ----

#### 1.4.2.2 Intensity ----

###### a) Three GPT words----

df1 <- df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(liking_Intensity_gpt = weighted.mean(liking_Intensity_avg, count))

df2 <- df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(disgust_Intensity_gpt = weighted.mean(disgust_Intensity_avg, count)) %>%
  select(disgust_Intensity_gpt)

tp <- cbind(df1, df2)

p3 <- tp %>% ggplot(aes(x = Year2)) +
  geom_point(aes(y = liking_Intensity_gpt), color = "black", size = 1) +
  geom_smooth(aes(y = liking_Intensity_gpt), color = 'black', span = 0.5, size = 0.5) +
  geom_point(aes(y = disgust_Intensity_gpt), color = "black", size =1, shape = 1) +
  geom_smooth(aes(y = disgust_Intensity_gpt), color = 'black', span = 0.5, linetype = 'dashed', size = 0.5)+
  labs(x = "(c)", y = "Liking (dots) vs. Disgust (circles)\navg. intensity")+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
  )+
  guides(color = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

p3

##### b) Lyric split----

Df1 <- Df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(liking_Intensity_split = weighted.mean(liking_Intensity_avg, count))

Df2 <- Df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(disgust_Intensity_split = weighted.mean(disgust_Intensity_avg, count)) %>%
  select(disgust_Intensity_split)

tp <- cbind(Df1, Df2)

p4 <- tp %>% ggplot(aes(x = Year2)) +
  geom_point(aes(y = liking_Intensity_split), color = "black", size = 1) +
  geom_smooth(aes(y = liking_Intensity_split), color = 'black', span = 0.5, size = 0.5) +
  geom_point(aes(y = disgust_Intensity_split), color = "black", size =1, shape = 1) +
  geom_smooth(aes(y = disgust_Intensity_split), color = 'black', span = 0.5, linetype = 'dashed', size = 0.5)+
  labs(x = "(d)", y = NULL)+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
  )+
  guides(color = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

p4

### 1.4.3 Anger and Fear ----

#### 1.4.3.2 Intensity ----

###### a) Three GPT words----

df1 <- df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(anger_Intensity_gpt = weighted.mean(anger_Intensity_avg, count))

df2 <- df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(fear_Intensity_gpt = weighted.mean(fear_Intensity_avg, count)) %>%
  select(fear_Intensity_gpt)

tp <- cbind(df1, df2)

p5 <- tp %>% ggplot(aes(x = Year2)) +
  geom_point(aes(y = anger_Intensity_gpt), color = "black", size = 1) +
  geom_smooth(aes(y = anger_Intensity_gpt), color = 'black', span = 0.5, size = 0.5) +
  geom_point(aes(y = fear_Intensity_gpt), color = "black", size =1, shape = 1) +
  geom_smooth(aes(y = fear_Intensity_gpt), color = 'black', span = 0.5, linetype = 'dashed', size = 0.5)+
  labs(x = "(e)", y = "Anger (dots) vs. Fear (circles)\navg. intensity")+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
  )+
  guides(color = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

p5

##### b) Lyric split----

Df1 <- Df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(anger_Intensity_split = weighted.mean(anger_Intensity_avg, count))

Df2 <- Df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(fear_Intensity_split = weighted.mean(fear_Intensity_avg, count)) %>%
  select(fear_Intensity_split)

tp <- cbind(Df1, Df2)

p6 <- tp %>% ggplot(aes(x = Year2)) +
  geom_point(aes(y = anger_Intensity_split), color = "black", size = 1) +
  geom_smooth(aes(y = anger_Intensity_split), color = 'black', span = 0.5, size = 0.5) +
  geom_point(aes(y = fear_Intensity_split), color = "black", size =1, shape = 1) +
  geom_smooth(aes(y = fear_Intensity_split), color = 'black', span = 0.5, linetype = 'dashed', size = 0.5)+
  labs(x = "(f)", y = NULL)+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
  )+
  guides(color = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

p6

### 1.4.4 Anticipation and Surprise ----

#### 1.4.4.2 Intensity ----

###### a) Three GPT words----

df1 <- df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(anticipation_Intensity_gpt = weighted.mean(anticipation_Intensity_avg, count))

df2 <- df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(surprise_Intensity_gpt = weighted.mean(surprise_Intensity_avg, count)) %>%
  select(surprise_Intensity_gpt)

tp <- cbind(df1, df2)

p7 <- tp %>% ggplot(aes(x = Year2)) +
  geom_point(aes(y = anticipation_Intensity_gpt), color = "black", size = 1) +
  geom_smooth(aes(y = anticipation_Intensity_gpt), color = 'black', span = 0.5, size = 0.5) +
  geom_point(aes(y = surprise_Intensity_gpt), color = "black", size =1, shape = 1) +
  geom_smooth(aes(y = surprise_Intensity_gpt), color = 'black', span = 0.5, linetype = 'dashed', size = 0.5)+
  labs(x = "(g)", y = "Anticipation (dots) vs. Surprise (circles)\navg. intensity")+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
  )+
  guides(color = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

p7

##### b) Lyric split----

Df1 <- Df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(anticipation_Intensity_split = weighted.mean(anticipation_Intensity_avg, count))

Df2 <- Df %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(surprise_Intensity_split = weighted.mean(surprise_Intensity_avg, count)) %>%
  select(surprise_Intensity_split)

tp <- cbind(Df1, Df2)

p8 <- tp %>% ggplot(aes(x = Year2)) +
  geom_point(aes(y = anticipation_Intensity_split), color = "black", size = 1) +
  geom_smooth(aes(y = anticipation_Intensity_split), color = 'black', span = 0.5, size = 0.5) +
  geom_point(aes(y = surprise_Intensity_split), color = "black", size =1, shape = 1) +
  geom_smooth(aes(y = surprise_Intensity_split), color = 'black', span = 0.5, linetype = 'dashed', size = 0.5)+
  labs(x = "(h)", y = NULL)+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )+
  guides(color = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

p8

### 1.4.5 Combined together ----

P <- (p1 + p2) / (p3 + p4) / (p5 + p6) / (p7 + p8)
P

ggsave("Fig 5.tiff", plot = P, width = 7, height = 10, dpi = 300, units = "in", compression = "lzw")


## 1.5 Trends of the four emotional clusters ----

### 1.5.1 Clustering on three GPT words, emotion percentage----

emo_colors <- c(
  "Bittersweet-love" = "gray95",
  "Happy-romance" = "gray80",
  "Sad-romance" = "gray40",
  "Resentful-heartbreak" = "gray10"
)

dF$clusterLab2[dF$clusterLab == "liking>>joy>sadness>anticip"] <- "Bittersweet-love"
dF$clusterLab2[dF$clusterLab == "joy>>liking"] <- "Happy-romance"
dF$clusterLab2[dF$clusterLab == "sadness>>anticip>>liking"] <- "Sad-romance"
dF$clusterLab2[dF$clusterLab == "disgust>sadness>>liking"] <- "Resentful-heartbreak"


tp <- dF %>% group_by(Year2, clusterLab2) %>%
  summarise(n = n()) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Emotion_cluster = clusterLab2) %>%
  mutate(Emotion_cluster = factor(Emotion_cluster,levels = c("Resentful-heartbreak",
                                                             "Happy-romance",
                                                             "Bittersweet-love",
                                                             "Sad-romance")))



p1 <- tp %>% ggplot(aes(x = Year2, y = Percentage, fill = Emotion_cluster)) +
  geom_area(alpha = 0.6, size = 0.2, colour = 'white') +
  theme_minimal() +
  labs(x = "(a)", y = "Percentages of emotion themes", title = "ChatGPT + Lexicon") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  )+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent) +
  scale_fill_manual(values = emo_colors, name = NULL)

p1

dF %>% group_by(clusterLab2) %>%
  summarise(n = n()) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Emotion_cluster = clusterLab2) %>%
  mutate(Emotion_cluster = factor(Emotion_cluster,levels = c("Resentful-heartbreak",
                                                             "Happy-romance",
                                                             "Bittersweet-love",
                                                             "Sad-romance")))

### 1.5.2 Clustering on lyric split, emotion percentage----

emo_colors <- c(
  "Bittersweet-love" = "gray95",
  "Happy-romance" = "gray80",
  "Sad-romance" = "gray40",
  "Resentful-heartbreak" = "gray10"
)

DF$clusterLab2[DF$clusterLab == "liking>>joy>sadness>disgust"] <- "Bittersweet-love"
DF$clusterLab2[DF$clusterLab == "joy>liking>>disgust>sadness"] <- "Happy-romance"
DF$clusterLab2[DF$clusterLab == "sadness>>liking>>disgust>anticip"] <- "Sad-romance"
DF$clusterLab2[DF$clusterLab == "disgust>>liking>>sadness>joy"] <- "Resentful-heartbreak"


tp <- DF %>% group_by(Year2, clusterLab2) %>%
  summarise(n = n()) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Emotion_cluster = clusterLab2) %>%
  mutate(Emotion_cluster = factor(Emotion_cluster,levels = c("Resentful-heartbreak",
                                                             "Happy-romance",
                                                             "Bittersweet-love",
                                                             "Sad-romance")))

p2 <- tp %>% ggplot(aes(x = Year2, y = Percentage, fill = Emotion_cluster)) +
  geom_area(alpha = 0.6, size = 0.2, colour = 'white') +
  theme_minimal() +
  labs(x = "(b)", y = NULL, title = "Lexicon Only") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 12)
  )+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent) +
  scale_fill_manual(values = emo_colors, name = NULL)

p2

DF %>% group_by(clusterLab2) %>%
  summarise(n = n()) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Emotion_cluster = clusterLab2) %>%
  mutate(Emotion_cluster = factor(Emotion_cluster,levels = c("Resentful-heartbreak",
                                                             "Happy-romance",
                                                             "Bittersweet-love",
                                                             "Sad-romance")))

### 1.5.3 Combined together----

P <- p1 + p2
P

ggsave("Fig a.tiff", plot = P, width = 8, height = 4, dpi = 300, units = "in", compression = "lzw")

## 1.6 Trends of emotional complexities: Emotional Richness Conflictedness ----

### 1.6.1 Three GPT words ----

tp <- dF %>% select(id, IntensityWithSign, threeEmotionsCH, Year2, singerType, region_coarse, joy_Intensity, liking_Intensity, anger_Intensity, sadness_Intensity, fear_Intensity, disgust_Intensity, surprise_Intensity, anticipation_Intensity, pos_Intensity, neg_Intensity) %>%
  mutate(denominator1 = joy_Intensity + liking_Intensity + anger_Intensity + sadness_Intensity + 
           fear_Intensity + disgust_Intensity + surprise_Intensity + anticipation_Intensity,
         numerator1 = abs(joy_Intensity - liking_Intensity) + abs(joy_Intensity - anger_Intensity) + abs(joy_Intensity - sadness_Intensity) + abs(joy_Intensity - fear_Intensity) + abs(joy_Intensity - disgust_Intensity) + abs(joy_Intensity - surprise_Intensity) + abs(joy_Intensity - anticipation_Intensity) +
           abs(liking_Intensity - anger_Intensity) + abs(liking_Intensity - sadness_Intensity) + abs(liking_Intensity - fear_Intensity) + abs(liking_Intensity - disgust_Intensity) + abs(liking_Intensity - surprise_Intensity) + abs(liking_Intensity - anticipation_Intensity) +
           abs(anger_Intensity - sadness_Intensity) + abs(anger_Intensity - fear_Intensity) + abs(anger_Intensity - disgust_Intensity) + abs(anger_Intensity - surprise_Intensity) + abs(anger_Intensity - anticipation_Intensity) +
           abs(sadness_Intensity - fear_Intensity) + abs(sadness_Intensity - disgust_Intensity) + abs(sadness_Intensity - surprise_Intensity) + abs(sadness_Intensity - anticipation_Intensity) +
           abs(fear_Intensity - disgust_Intensity) + abs(fear_Intensity - surprise_Intensity) + abs(fear_Intensity - anticipation_Intensity) +
           abs(disgust_Intensity - surprise_Intensity) + abs(disgust_Intensity - anticipation_Intensity) +
           abs(surprise_Intensity - anticipation_Intensity),
         rho = 1 - (numerator1 / 7 / denominator1),
         # richness = rho * (pos_Intensity + neg_Intensity),
         kappa =1 - abs(pos_Intensity - neg_Intensity) / (pos_Intensity + neg_Intensity),
         # conflictedness = kappa * (pos_Intensity + neg_Intensity)
  )

tp1 <- tp %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(var_rho = var(rho), var_kappa = var(kappa), rho = mean(rho), kappa = mean(kappa))

p1 <- tp1 %>% ggplot(aes(x = Year2)) +
  geom_point(aes(y = rho),  color = "black", size = 1) +
  geom_smooth(aes(y = rho), color = 'black', span = 0.5, size = 0.5) +
  labs(x = "(a)", y = "Avg. emotional richness", title = "ChatGPT + Lexicon")+
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))+
  guides(color = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

p1

p3 <- tp1 %>% ggplot(aes(x = Year2)) +
  geom_point(aes(y = kappa), color = "black", size =1) +
  geom_smooth(aes(y = kappa), color = 'black', span = 0.5, size = 0.5)+
  labs(x = "(a)", y = "Avg. emotional conflictedness", title = "ChatGPT + Lexicon")+
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))+
  guides(color = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

p3

### 1.6.2 Lyric split ----

tp <- DF %>% select(id, IntensityWithSign, Year2, singerType, region_coarse, joy_Intensity, liking_Intensity, anger_Intensity, sadness_Intensity, fear_Intensity, disgust_Intensity, surprise_Intensity, anticipation_Intensity, pos_Intensity, neg_Intensity) %>%
  mutate(denominator1 = joy_Intensity + liking_Intensity + anger_Intensity + sadness_Intensity + 
           fear_Intensity + disgust_Intensity + surprise_Intensity + anticipation_Intensity,
         numerator1 = abs(joy_Intensity - liking_Intensity) + abs(joy_Intensity - anger_Intensity) + abs(joy_Intensity - sadness_Intensity) + abs(joy_Intensity - fear_Intensity) + abs(joy_Intensity - disgust_Intensity) + abs(joy_Intensity - surprise_Intensity) + abs(joy_Intensity - anticipation_Intensity) +
           abs(liking_Intensity - anger_Intensity) + abs(liking_Intensity - sadness_Intensity) + abs(liking_Intensity - fear_Intensity) + abs(liking_Intensity - disgust_Intensity) + abs(liking_Intensity - surprise_Intensity) + abs(liking_Intensity - anticipation_Intensity) +
           abs(anger_Intensity - sadness_Intensity) + abs(anger_Intensity - fear_Intensity) + abs(anger_Intensity - disgust_Intensity) + abs(anger_Intensity - surprise_Intensity) + abs(anger_Intensity - anticipation_Intensity) +
           abs(sadness_Intensity - fear_Intensity) + abs(sadness_Intensity - disgust_Intensity) + abs(sadness_Intensity - surprise_Intensity) + abs(sadness_Intensity - anticipation_Intensity) +
           abs(fear_Intensity - disgust_Intensity) + abs(fear_Intensity - surprise_Intensity) + abs(fear_Intensity - anticipation_Intensity) +
           abs(disgust_Intensity - surprise_Intensity) + abs(disgust_Intensity - anticipation_Intensity) +
           abs(surprise_Intensity - anticipation_Intensity),
         rho = 1 - (numerator1 / 7 / denominator1),
         # richness = rho * (pos_Intensity + neg_Intensity),
         kappa =1 - abs(pos_Intensity - neg_Intensity) / (pos_Intensity + neg_Intensity),
         # conflictedness = kappa * (pos_Intensity + neg_Intensity)
  )

tp1 <- tp %>% filter(Year2 >= 1967) %>%
  group_by(Year2) %>%
  summarise(var_rho = var(rho), var_kappa = var(kappa), rho = mean(rho), kappa = mean(kappa))

p2 <- tp1 %>% ggplot(aes(x = Year2)) +
  geom_point(aes(y = rho),  color = "black", size = 1) +
  geom_smooth(aes(y = rho), color = 'black', span = 0.5, size = 0.5) +
  labs(x = "(b)", y = NULL, title = "Lexicon Only")+
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))+
  guides(color = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

p2

p4 <- tp1 %>% ggplot(aes(x = Year2)) +
  geom_point(aes(y = kappa), color = "black", size =1) +
  geom_smooth(aes(y = kappa), color = 'black', span = 0.5, size = 0.5)+
  labs(x = "(b)", y = NULL, title = "Lexicon Only")+
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))+
  guides(color = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

p4

### 1.6.3 Combined together ----

P <- p1 + p2
P

ggsave("Fig 8.tiff", plot = P, width = 6, height = 3, dpi = 300, units = "in", compression = "lzw")

P <- p3 + p4
P

ggsave("Fig 9.tiff", plot = P, width = 6, height = 3, dpi = 300, units = "in", compression = "lzw")


# 2. Snapshots ----

## 2.1 Radar maps for the eight emotions----

### 2.1.1 Radar maps for 1980, 1984, 1995, for 3 GPT words----

df1 <- dF %>% filter(Year2 == 1980 | Year2 == 1984 | Year2 == 1990) %>%
  group_by(Year2) %>%
  summarise(Positive = mean(pos_Intensity),
            Joy = mean(joy_Intensity),
            Liking = mean(liking_Intensity),
            Anticipation = mean(anticipation_Intensity),
            Anger = mean(anger_Intensity),
            Negative = mean(neg_Intensity),
            Sadness = mean(sadness_Intensity),
            Disgust = mean(disgust_Intensity),
            Surprise = mean(surprise_Intensity),
            Fear = mean(fear_Intensity)
  )

group = c('1980', '1984', '1990')

df0 <- cbind(group, df1)
rownames(df0) <- group
df0 <- df0[,-c(2)]

max <- rep(8, 10)
p <- ggradar2(df0, fullscore = max, 
              group.line.width = 1, 
              group.point.size = 2, 
              axis.label.offset = 1.1,
              gridline.label = seq(0, 8, 2),
              gridline.label.type = "numeric",
              background.circle.colour = "white",
              background.circle.transparency = 0.1,
              polygonfill = FALSE,
              gridline.mid.colour = "grey",
              plot.title = "ChatGPT + Lexicon")

# Modify title font size and position
p1 <- p + 
  scale_color_grey(start = 0.7, end = 0.1) + # grayscale
  labs(caption = "(a)") +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, vjust = 0),
    plot.caption = element_text(size = 22, hjust = 0.5, vjust = 0)
  )

p1

### 2.1.2 Radar maps for 1980, 1984, 1995, for Lyric split----

df1 <- DF %>% filter(Year2 == 1977 | Year2 == 1983 | Year2 == 1987) %>%
  group_by(Year2) %>%
  summarise(Positive = mean(pos_Intensity),
            Joy = mean(joy_Intensity),
            Liking = mean(liking_Intensity),
            Anticipation = mean(anticipation_Intensity),
            Anger = mean(anger_Intensity),
            Negative = mean(neg_Intensity),
            Sadness = mean(sadness_Intensity),
            Disgust = mean(disgust_Intensity),
            Surprise = mean(surprise_Intensity),
            Fear = mean(fear_Intensity)
  )

# group = c('1977', '1983', '1987')

df0 <- cbind(group, df1)
rownames(df0) <- group
df0 <- df0[,-c(2)]

max <- rep(25, 10)
p <- ggradar2(df0, fullscore = max, 
         # radarshape = 'sharp', 
         group.line.width = 1, 
         group.point.size = 2, 
         axis.label.offset = 1.1,
         gridline.label = seq(0, 25, 5),
         gridline.label.type = "numeric",
         background.circle.colour = "white",
         background.circle.transparency = 0.1,
         gridline.mid.colour = "grey",
         polygonfill = F,
         plot.title = "Lexicon Only",
         plot.legend = F)

# Modify title font size and position
p2 <- p + 
  scale_color_grey(start = 0.7, end = 0.1) + # grayscale
  labs(caption = "(b)") +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, vjust = 0),
    plot.caption = element_text(size = 22, hjust = 0.5, vjust = 0)
  )

p2

### 2.1.3 Combined together----

P <- p1 + p2
P
ggsave("Fig 6.tiff", plot = P, width = 14, height = 7, dpi = 300, units = "in", compression = "lzw")

## 2.2 Radar maps for the four emotion clusters----

### 2.2.1 Three GPT words intensity----

emo_colors <- c(
  "Resentful-heartbreak" = "gray35",   # Very dark gray (almost black)
  "Happy-romance" = "gray60",        # Medium-dark gray
  "Bittersweet-love" = "gray80",     # Medium-light gray
  "Sad-romance" = "gray0"     # Very light gray
)

df0 <- data.frame(
  group    = c("Resentful-heartbreak", "Happy-romance", "Bittersweet-love", "Sad-romance"),
  Joy      = c(0.06338764, 0.5837455, 0.1859476, 0.05532933),
  Liking   = c(0.10020475, 0.27892975, 0.57175782, 0.19067233),
  Anticipation  = c(0.03461098, 0.04357636, 0.14719702, 0.25915811),
  Anger    = c(0.04721393, 0.00891785, 0.00555592, 0.0066258),
  Sadness  = c(0.27853998, 0.06280159, 0.17190283, 0.64271089),
  Disgust  = c(0.44751062, 0.04435246, 0.0453448, 0.08881245),
  Surprise = c(0.00851572, 0.00894011, 0.00450627, 0.00191593),
  Fear     = c(0.05462736, 0.01231273, 0.01498476, 0.01393328)
)

max <- rep(0.8, 8)

p <- ggradar2(df0, fullscore = max, 
         group.line.width = 1, 
         group.point.size = 2, 
         axis.label.offset = 1.1,
         gridline.label = seq(0, .8, .2),
         gridline.label.type = "numeric",
         background.circle.colour = "white",
         background.circle.transparency = 0.1,
         gridline.mid.colour = "grey",
         polygonfill = FALSE,
         plot.title = "ChatGPT + Lexicon")

p1 <- p + 
  scale_color_manual(values = emo_colors) +  # Use manually defined grayscale colors
  labs(caption = "(a)") +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, vjust = 0),
    plot.caption = element_text(size = 22, hjust = 0.5, vjust = 0)
  )

p1

### 2.2.2 Lyric split intensity----

emo_colors <- c(
  "Resentful-heartbreak" = "gray35",   # Very dark gray (almost black)
  "Happy-romance" = "gray60",        # Medium-dark gray
  "Bittersweet-love" = "gray80",     # Medium-light gray
  "Sad-romance" = "gray0"     # Very light gray
)

df0 <- data.frame(
  group    = c("Resentful-heartbreak", "Happy-romance", "Bittersweet-love", "Sad-romance"),
  Joy      = c(0.1088741, 0.37517674, 0.12041773, 0.10465214),
  Liking   = c(0.27272397, 0.34357661, 0.62550439, 0.2895999),
  Anticipation  = c(0.02856086, 0.03814319, 0.04714579, 0.10097668),
  Anger    = c(0.01615175, 0.0055324, 0.00442461, 0.0062236),
  Sadness  = c(0.16199366, 0.11480883, 0.1108608, 0.40171701),
  Disgust  = c(0.37634933, 0.11786384, 0.10578959, 0.15857277),
  Surprise = c(0.01517115, 0.01663488, 0.01125593, 0.00955973),
  Fear     = c(0.04873604, 0.0264022, 0.02174694, 0.02967486)
)

max <- rep(.8, 8)

p <- ggradar2(df0, fullscore = max, 
              group.line.width = 1, 
              group.point.size = 2, 
              axis.label.offset = 1.1,
              gridline.label = seq(0, .8, .2),
              gridline.label.type = "numeric",
              background.circle.colour = "white",
              background.circle.transparency = 0.1,
              gridline.mid.colour = "grey",
              polygonfill = FALSE,
              plot.title = "Lexicon Only",
              plot.legend = F)

p2 <- p + 
  scale_color_manual(values = emo_colors) +  # Use manually defined grayscale colors
  labs(caption = "(b)") +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, vjust = 0),
    plot.caption = element_text(size = 22, hjust = 0.5, vjust = 0)
  )

p2

### 2.2.3 Combined together----

P <- p1 + p2
P
ggsave("Fig 7.tiff", plot = P, width = 16, height = 8, dpi = 300, units = "in", compression = "lzw")

## 2.3 By singerType: Radar maps for the eight emotions----

### 2.3.1 By singerType: 3 GPT words----

df1 <- dF %>% 
  mutate(singerType = factor(singerType, levels = c('group', 'male', 'female'))) %>%
  group_by(singerType) %>%
  summarise(Positive = mean(pos_Intensity),
            Joy = mean(joy_Intensity),
            Liking = mean(liking_Intensity),
            Anticipation = mean(anticipation_Intensity),
            Anger = mean(anger_Intensity),
            Negative = mean(neg_Intensity),
            Sadness = mean(sadness_Intensity),
            Disgust = mean(disgust_Intensity),
            Surprise = mean(surprise_Intensity),
            Fear = mean(fear_Intensity)
  )

group = c('group', 'male', 'female')

df0 <- cbind(group, df1)
rownames(df0) <- group
df0 <- df0[,-c(2)]

max <- rep(8, 10)
p <- ggradar2(df0, fullscore = max, 
              group.line.width = 1, 
              group.point.size = 2, 
              axis.label.offset = 1.1,
              gridline.label = seq(0, 8, 2),
              gridline.label.type = "numeric",
              background.circle.colour = "white",
              background.circle.transparency = 0.1,
              polygonfill = FALSE,
              gridline.mid.colour = "grey",
              plot.title = "ChatGPT + Lexicon")

# Modify title font size and position

gray_shades <- c(
  "group" = "gray20",
  "male" = "gray50",
  "female" = "gray80"
)

p1 <- p + 
  scale_color_manual(values = gray_shades,
                     breaks = c("group", "male", "female")) +
  labs(caption = "(a)") +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, vjust = 0),
    plot.caption = element_text(size = 22, hjust = 0.5, vjust = 0)
  )

p1

### 2.3.2 By singerType: Lyric split----

df1 <- DF %>% 
  mutate(singerType = factor(singerType, levels = c('group', 'male', 'female'))) %>%
  group_by(singerType) %>%
  summarise(Positive = mean(pos_Intensity),
            Joy = mean(joy_Intensity),
            Liking = mean(liking_Intensity),
            Anticipation = mean(anticipation_Intensity),
            Anger = mean(anger_Intensity),
            Negative = mean(neg_Intensity),
            Sadness = mean(sadness_Intensity),
            Disgust = mean(disgust_Intensity),
            Surprise = mean(surprise_Intensity),
            Fear = mean(fear_Intensity)
  )

group = c('group', 'male', 'female')

df0 <- cbind(group, df1)
rownames(df0) <- group
df0 <- df0[,-c(2)]

max <- rep(20, 10)
p <- ggradar2(df0, fullscore = max, 
              # radarshape = 'sharp', 
              group.line.width = 1, 
              group.point.size = 2, 
              axis.label.offset = 1.1,
              gridline.label = seq(0, 20, 5),
              gridline.label.type = "numeric",
              background.circle.colour = "white",
              background.circle.transparency = 0.1,
              gridline.mid.colour = "grey",
              polygonfill = F,
              plot.title = "Lexicon Only",
              plot.legend = F)

# Modify title font size and position

gray_shades <- c(
  "group" = "gray20",
  "male" = "gray50",
  "female" = "gray80"
)

p2 <- p + 
  scale_color_manual(values = gray_shades,
                     breaks = c("group", "male", "female")) +
  labs(caption = "(b)") +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, vjust = 0),
    plot.caption = element_text(size = 22, hjust = 0.5, vjust = 0)
  )

p2

### 2.3.3 Combined together----

P <- p1 + p2
P
ggsave("Fig 15.tiff", plot = P, width = 14, height = 7, dpi = 300, units = "in", compression = "lzw")


## 2.4 Chord graphs for the eight emotions----

# Defining functions

prep_chord_data <- function(df, year = NULL, region_ = NULL, singerType_ = NULL, num_chords = 3, include_self = FALSE, emoList) {
  
  all_emotions_data <- list()
  
  for (emotion in emoList) {
    chord_data <- df %>%
      select("Year2", "region", "singerType", all_of(emoList)) %>%
      filter(.data[[emotion]] > 0)  # Use .data for tidy evaluation
    
    if (!is.null(year)) {
      chord_data <- chord_data %>%
        filter(Year2 == year)
    }
    
    if (!is.null(region_)) {
      chord_data <- chord_data %>%
        filter(region == region_)
    }
    
    if (!is.null(singerType_)) {
      chord_data <- chord_data %>%
        filter(singerType == singerType_)
    }
    
    # if (include_self) {
    #   chord_data <- chord_data %>%
    #     mutate(!!emotion := .data[[emotion]] - 1)
    # }
    
    chord_data <- chord_data %>%
      summarise(across(all_of(emoList), sum, .names = "{col}")) %>%
      mutate(total_count = rowSums(select(., all_of(emoList))),  # Create a new column for total count
             across(all_of(emoList), ~ . / total_count, .names = "{.col}_perc")) %>%
      mutate(emotion = emotion) %>%  # Add a column to indicate the emotion
      ungroup()
    
    all_emotions_data[[emotion]] <- chord_data
  }
  
  # Combine all the emotion-specific data frames into one data frame
  final_df <- bind_rows(all_emotions_data)
  
  # Select only the emotion columns and convert to a matrix
  proportion_columns <- final_df$emotion
  
  matrix_data <- final_df %>%
    select(all_of(proportion_columns)) %>%
    as.matrix()
  
  if (include_self == FALSE) {
    diag(matrix_data) <- 0
  }
  
  for (i in 1:nrow(matrix_data)) {
    row_values <- matrix_data[i, ]
    top_n_indices <- order(row_values, decreasing = TRUE)[1:num_chords]
    matrix_data[i, -top_n_indices] <- 0
  }
  
  # Set row names as the emotions
  rownames(matrix_data) <- final_df$emotion
  
  return(matrix_data)
}

# Define the new function chordDiagram2
chordDiagram2 <- function(x, transparency = 0.5, grid.col, col, annotationTrack = "grid", ...) {
  # Set parameters to increase the size of the plotting area
  circos.par(canvas.xlim = c(-1.2, 1.2), canvas.ylim = c(-1.2, 1.2))
  
  # Create the chord diagram without the original labels
  chordDiagram(
    x = x,
    transparency = transparency,
    grid.col = grid.col,
    col = col,
    annotationTrack = annotationTrack,
    ...
  )
  
  # Function to format tick labels in scientific notation
  scientific_label <- function(x) {
    format(x, scientific = TRUE)
  }
  
  # Add axis ticks and labels in scientific notation
  circos.trackPlotRegion(
    ylim = c(0, 1),
    track.height = 0.05,
    bg.border = NA,
    panel.fun = function(x, y) {
      circos.axis(
        h = "top",
        labels.cex = 0.6,  # Adjust this value to change the text size of the ticks
        major.tick.length = 3.7,  # Adjust this value to change the length of the ticks
        labels.facing = "clockwise",
        direction = "outside",
        labels = scientific_label
      )
    }
  )
  
  # Add new labels outside the circle
  circos.trackPlotRegion(
    ylim = c(0, 1),
    track.height = 0.05,
    bg.border = NA,
    track.margin = c(0.02, 0.02),
    panel.fun = function(x, y) {
      circos.text(
        CELL_META$xcenter,
        CELL_META$ylim[2] + 14,  # Adjust position outward
        CELL_META$sector.index,
        facing = "outside",
        niceFacing = TRUE,
        adj = c(0.5, 0.5),
        cex = 1.4  # Adjust this value to increase the text size
      )
    }
  )
  
  # Clear the plotting environment to avoid overlaying subsequent plots
  circos.clear()
}

# Define the base color palette for emotions
# base_colors <- c(
#   "liking" = "pink",
#   "joy" = "#FF6019",
#   "positive" = "#c4001f",
#   "surprise" = "#C0C0C0",
#   "anticipation" = "#7E4D34",
#   "sadness" = "#4169E1",
#   "anger" = "#134f5c",
#   "disgust" = "#6aa84f",
#   "fear" = "#674ea7",
#   "negative" = "#091414"
# )


base_colors <- c(
  "liking" = "pink",
  "joy" = "#FF6019",
  "surprise" = "#674ea7",
  "anticipation" = "#FFD700",
  "sadness" = "#4169E1",
  "anger" = "#c4001f",
  "disgust" = "#6aa84f",
  "fear" = "#091414"
)

# Create a color palette for the specific measure
color_palette <- base_colors

# Create a data frame for link colors
link_colors_df <- data.frame(
  from = rep(names(color_palette), each = length(color_palette)),
  to = rep(names(color_palette), length(color_palette)),
  colors = rep("#000000", length(color_palette) ^ 2) # Initialize with black color
)

# Assign colors to specific links
for (i in 1:length(color_palette)) {
  from_emotion <- names(color_palette)[i]
  link_colors_df$colors[link_colors_df$from == from_emotion] <- color_palette[from_emotion]
}


emolist <- c("fear", "disgust", "surprise", "sadness", "anger", "anticipation", "liking", "joy")
# emolist <- c("positive", "fear", "disgust", "surprise", "sadness", "negative", "anger", "anticipation", "liking", "joy")

year <- NULL
# year <- 2020

region <- NULL
# region <- "Taiwan"
# region <- list(tw = "Taiwan")
# region <- list(mlc = "China-Main", hk = "Hong Kong", tw = "Taiwan", gd = "Guangdong", xz = "Xizang", xj = "Xinjiang", nm = "Neimeng", my = "Malaysia", sg = "Singapore", mc = "Macao")

singerType <- NULL
# singerType <- "male"

### 2.4.1 Chord graph for 3 GPT words----

chord_data <- dF %>%
  rename(positive = pos_ct, negative = neg_ct) %>%
  prep_chord_data(emoList = emolist,
                  year = year, 
                  region_ = region,
                  singerType_ = singerType, 
                  num_chords = 8, 
                  include_self = T
  )

chord_gpt <- as.data.frame(chord_data)

chord_gpt <- cbind(RowName = rownames(chord_gpt), chord_gpt)
write.xlsx(chord_gpt, file = "chord_gpt.xlsx")

# Ensure dF contains the required columns
chord_data <- dF %>%
  rename(positive = pos_ct, negative = neg_ct) %>%
  prep_chord_data(emoList = emolist,
                  year = year, 
                  region_ = region,
                  singerType_ = singerType, 
                  num_chords = 3, 
                  include_self = F
  )

# Create the chord diagram with the custom colors
tiff("Fig j.tiff", width = 8, height = 8, units = "in", res = 300)
chordDiagram2(
  x = chord_data,
  transparency = 0.5,
  grid.col = color_palette,
  col = link_colors_df
)
dev.off()



### 2.4.2 Chord graph for lyric split----

chord_data <- DF %>%
  rename(positive = pos_ct, negative = neg_ct) %>%
  prep_chord_data(emoList = emolist,
                  year = year, 
                  region_ = region,
                  singerType_ = singerType, 
                  num_chords = 8, 
                  include_self = T
  )

chord_lyric_split <- apply(chord_data, c(1, 2), function(x) as.integer(round(x)))
chord_lyric_split <- as.data.frame(chord_lyric_split)

chord_lyric_split <- cbind(RowName = rownames(chord_lyric_split), chord_lyric_split)
write.xlsx(chord_lyric_split, file = "chord_lyric_split.xlsx")


chord_data <- DF %>%
  rename(positive = pos_ct, negative = neg_ct) %>%
  prep_chord_data(emoList = emolist,
                  year = year, 
                  region_ = region,
                  singerType_ = singerType, 
                  num_chords = 3, 
                  include_self = F
  )

# Create the chord diagram with the custom colors
tiff("Fig k.tiff", width = 8, height = 8, units = "in", res = 300)
chordDiagram2(
  x = chord_data,
  transparency = 0.5,
  grid.col = color_palette,
  col = link_colors_df
)
dev.off()


temp_lyric_split <- chord_data

## 2.5 Edge bundling for sub-emotions----

### 2.5.1 Edge bundling for 3 GPT words----

#### 2.5.1.1 Creating edges, vertices, and connections----

edges <- read_csv("edges.csv")
# write_csv(edges, "edges.csv")

# create a vertices data.frame. One line per object of our hierarchy
vertices  <-  data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))), 
  value = NA
) 

# Calculate the frequency of each sub-emotion, used later for determining the sizes of the nodes

tp <- dF %>% select(PA, PE, PD, PH, PG, PB, PK, PF, NPF, NAA, NJ, NB, NH, NE, ND, NN, NK, NL, NPC, PC, NI, NC, NG) %>%
  rename(happiness_PA = PA,
         contentment_PE = PE,
         respect_PD = PD,
         praise_PH = PH,
         trust_PG = PG,
         fondness_PB = PB,
         wishing_PK = PK,
         longing_PF = PF,
         missing_NPF = NPF,
         anger_NA = NAA,
         sorrow_NB = NB,
         disappointment_NJ = NJ,
         guilt_NH = NH,
         annoyance_NE = NE,
         aversion_ND =ND,
         criticism_NN = NN,
         envy_NK = NK,
         doubt_NL = NL,
         amazement_PC = PC,
         shock_NPC = NPC,
         panic_NI = NI,
         dread_NC = NC,
         shame_NG = NG
  )

vec <- colSums(tp)

vec <-vec/max(vec)*100 + 2 # normalize
vertices$value[10:32] <- vec

# Add a column with the group of each name. It will be useful later to color points
vertices$group  <-  edges$from[ match( vertices$name, edges$to ) ]
vertices$group[vertices$name == "origin"] <- "zero"

# Create connections between sub-emotions 
# The connections are created in a separate R script "Parallel processing for calculating edge connections in 3 GPT words.R"
# Only need to read in the created file.

connect <- read_csv("subEmo_connections_gpt.csv")

connect2 <- connect %>% group_by(from, to) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

set.seed(1234)
connect3 <- connect %>% group_by(from, to) %>%
  sample_frac(0.001)

connect4 <- connect3 %>% group_by(from, to) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

set.seed(1234)
connect5 <- connect3 %>% group_by(from, to) %>%
  sample_frac(0.05) # this fraction is set to 0.05, 0.1, 0.2, respectively

connect6 <- connect5 %>% group_by(from, to) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

#### 2.5.1.2 Creating leaf and branch labels----

# Add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
#calculate the ANGLE of the leaf labels
myleaves <- which(is.na( match(vertices$name, edges$from) ))
nleaves <- length(myleaves)
vertices$id[ myleaves ] <- seq(1:nleaves)
vertices$angle <- 90 - 360 * vertices$id / nleaves

# calculate the alignment of the leaf labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust <- ifelse( vertices$angle < -90, 1, 0)

# flip some leaf angles to make them readable
vertices$angle <- ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)

#calculate the Angle of the branch labels
nbranches <- length(vertices$group[vertices$group == "origin"]) 

original_order <- unique(edges$from) # Store the original order of the 'from' column
edges$from <- factor(edges$from, levels = original_order) # Convert 'from' to a factor with levels in the original order

tp1 <- edges %>% 
  group_by(from) %>%
  summarise(nSubEmo = n()) # Group by 'from' and summarise without changing the original order
tp1$nSubEmo[1] <- 0

nSubEmo_cum <- 0
for (i in 2:9) {
  vertices$angle[i] <- -(nSubEmo_cum  + tp1$nSubEmo[i]/2)*360/nleaves
  nSubEmo_cum <- nSubEmo_cum + tp1$nSubEmo[i]
}

# flip some branch angles to make them readable
vertices$angle[vertices$group == "origin"] <- ifelse(vertices$angle[vertices$group == "origin"] < -90 & vertices$angle[vertices$group == "origin"] > -270, 
                                                     vertices$angle[vertices$group == "origin"]+180, 
                                                     vertices$angle[vertices$group == "origin"])



#### 2.5.1.3 Plotting----

custom_palette <- c(
  "liking" = "hotpink",
  "joy" = "#FF6019",
  "surprise" = "#674ea7",
  "anticipation" = "#FFD700",
  "sadness" = "#4169E1",
  "anger" = "#c4001f",
  "disgust" = "#6aa84f",
  "fear" = "#091414"
)

mygraph <- graph_from_data_frame(edges, vertices=vertices )

# The connection object must refer to the ids of the leaves:
from  <-  match( connect5$from, vertices$name)
to  <-  match( connect5$to, vertices$name)

ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal() +
  theme_void()

p1 <- ggraph(mygraph, layout = 'dendrogram', circular = TRUE)
p1 <- p1 + geom_conn_bundle(data = get_con(from = from, to = to), alpha = 0.6, colour="gray60", width = 0.9, tension = 0.7)
p1 <- p1 + geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=3.5, alpha=1)
p1 <- p1 + geom_node_text(aes(x = x*3.1, y=y*3.1, filter = group == "origin", label=name, angle = angle, hjust=0.5, colour = name), size=6, alpha=1)
p1 <- p1 + geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value), alpha= 0.5)
p1 <- p1 + scale_colour_manual(values = custom_palette)
p1 <- p1 + scale_size_continuous(range = c(2,12))
p1 <- p1 + theme_void()
p1 <- p1 + theme(legend.position="none", 
                 plot.margin=unit(c(0,0,0,0),"cm"),
                 plot.background = element_rect(fill = "white", color = NA),   # Force white background
                 panel.background = element_rect(fill = "white", color = NA))   # Force white background)
p1 <- p1 + expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))

p1

ggsave("Fig l.tiff", plot = p1, width = 8, height = 8, dpi = 300, units = "in", compression = "lzw")

p1 <- p1 + labs(title = "ChatGPT + Lexicon", caption = "(a)") +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, vjust = 0),
    plot.caption = element_text(size = 18, hjust = 0.5, vjust = 1.5)
  )


### 2.5.2 Edge bundling for lyric split----

#### 2.5.2.1 Creating edges, vertices, and connections----

edges <- read_csv("edges.csv")
# write_csv(edges, "edges.csv")

# create a vertices data.frame. One line per object of our hierarchy
vertices  <-  data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))), 
  value = NA
) 

# Calculate the frequency of each sub-emotion, used later for determining the sizes of the nodes

set.seed(1234)
tp <- DF %>% 
  # group_by(singerType, region, Year2) %>%
  # sample_frac(0.2) %>%
  select(PA, PE, PD, PH, PG, PB, PK, PF, NPF, NAA, NJ, NB, NH, NE, ND, NN, NK, NL, NPC, PC, NI, NC, NG) %>%
  rename(happiness_PA = PA,
         contentment_PE = PE,
         respect_PD = PD,
         praise_PH = PH,
         trust_PG = PG,
         fondness_PB = PB,
         wishing_PK = PK,
         longing_PF = PF,
         missing_NPF = NPF,
         anger_NA = NAA,
         sorrow_NB = NB,
         disappointment_NJ = NJ,
         guilt_NH = NH,
         annoyance_NE = NE,
         aversion_ND =ND,
         criticism_NN = NN,
         envy_NK = NK,
         doubt_NL = NL,
         amazement_PC = PC,
         shock_NPC = NPC,
         panic_NI = NI,
         dread_NC = NC,
         shame_NG = NG
  )


vec <- colSums(tp)

vec <-vec/max(vec)*100 + 2 # normalize
vertices$value[10:32] <- vec

# Add a column with the group of each name. It will be useful later to color points
vertices$group  <-  edges$from[ match( vertices$name, edges$to ) ]
vertices$group[vertices$name == "origin"] <- "zero"

# Create connections between sub-emotions
# The connections are created in a separate R script "Parallel processing for calculating edge connections in lyric split.R"
# Only need to read in the created file.

connect <- read_csv("subEmo_connections_lyric_split.csv")

connect2 <- connect %>% group_by(from, to) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

set.seed(1234)
connect3 <- connect %>% group_by(from, to) %>%
  sample_frac(0.0001)

connect4 <- connect3 %>% group_by(from, to) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

set.seed(1234)
connect5 <- connect3 %>% group_by(from, to) %>%
  sample_frac(0.1) # this fraction is set to 0.2, respectively

connect6 <- connect5 %>% group_by(from, to) %>%
  summarise(n = n()) %>%
  arrange(desc(n))


#### 2.5.2.2 Creating leaf and branch labels----

# Add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
#calculate the ANGLE of the leaf labels
myleaves <- which(is.na( match(vertices$name, edges$from) ))
nleaves <- length(myleaves)
vertices$id[ myleaves ] <- seq(1:nleaves)
vertices$angle <- 90 - 360 * vertices$id / nleaves

# calculate the alignment of the leaf labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust <- ifelse( vertices$angle < -90, 1, 0)

# flip some leaf angles to make them readable
vertices$angle <- ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)

#calculate the Angle of the branch labels
nbranches <- length(vertices$group[vertices$group == "origin"]) 

original_order <- unique(edges$from) # Store the original order of the 'from' column
edges$from <- factor(edges$from, levels = original_order) # Convert 'from' to a factor with levels in the original order

tp1 <- edges %>% 
  group_by(from) %>%
  summarise(nSubEmo = n()) # Group by 'from' and summarise without changing the original order
tp1$nSubEmo[1] <- 0

nSubEmo_cum <- 0
for (i in 2:9) {
  vertices$angle[i] <- -(nSubEmo_cum  + tp1$nSubEmo[i]/2)*360/nleaves
  nSubEmo_cum <- nSubEmo_cum + tp1$nSubEmo[i]
}

# flip some branch angles to make them readable
vertices$angle[vertices$group == "origin"] <- ifelse(vertices$angle[vertices$group == "origin"] < -90 & vertices$angle[vertices$group == "origin"] > -270, 
                                                     vertices$angle[vertices$group == "origin"]+180, 
                                                     vertices$angle[vertices$group == "origin"])


#### 2.5.2.3 Plotting----

custom_palette <- c(
  "liking" = "hotpink",
  "joy" = "#FF6019",
  "surprise" = "#674ea7",
  "anticipation" = "#FFD700",
  "sadness" = "#4169E1",
  "anger" = "#c4001f",
  "disgust" = "#6aa84f",
  "fear" = "#091414"
)

mygraph <- graph_from_data_frame(edges, vertices=vertices )

# The connection object must refer to the ids of the leaves:

from  <-  match( connect5$from, vertices$name)
to  <-  match( connect5$to, vertices$name)

p2 <- ggraph(mygraph, layout = 'dendrogram', circular = TRUE)
# Adding layers incrementally
p2 <- p2 + geom_conn_bundle(
  data = get_con(from = from, to = to), 
  alpha = 0.6, colour="gray60", width = 0.9, tension = 0.7
)
p2 <- p2 + geom_node_text(
  aes(x = x * 1.15, y = y * 1.15, filter = leaf, label = name, angle = angle, hjust = hjust, colour = group),
  size = 3.5, alpha = 1
)
p2 <- p2 + geom_node_text(
  aes(x = x * 3.1, y = y * 3.1, filter = group == "origin", label = name, angle = angle, hjust = 0.5, colour = name),
  size = 6, alpha = 1
)
p2 <- p2 + geom_node_point(
  aes(filter = leaf, x = x * 1.07, y = y * 1.07, colour = group, size = value),
  alpha = 0.5
)
p2 <- p2 + scale_colour_manual(values = custom_palette)
p2 <- p2 + scale_size_continuous(range = c(2, 12))
p2 <- p2 + theme_void()
p2 <- p2 + theme(
  legend.position = "none",
  plot.margin = unit(c(0, 0, 0, 0), "cm",),
  plot.background = element_rect(fill = "white", color = NA),   # Force white background
  panel.background = element_rect(fill = "white", color = NA)   # Force white background
)
p2 <- p2 + expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))

p2

ggsave("Fig m.tiff", plot = p2, width = 8, height = 8, dpi = 300, units = "in", compression = "lzw")

p2 <- p2 + labs(title = "Lexicon Only", caption = "(b)") +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, vjust = 0),
    plot.caption = element_text(size = 18, hjust = 0.5, vjust = 1.5)
  )

### 2.5.3 Combined together----

P <- p1 + p2
P
ggsave("Fig 11.tiff", plot = P, width = 16, height = 8, dpi = 300, units = "in", compression = "lzw")

### 2.5.4 Network Analysis (using inverse weights for betweenness and closeness centrality measures; should be the correct way!)----

#### a) Compute centrality measures for sub-emotions-----

# Create an igraph graph object with weights
g <- graph_from_data_frame(connect2, directed = FALSE)

# Set original edge weights
E(g)$weight <- connect2$n

# Compute Min-Max normalized inverse weights for shortest-path-based measures
# max_weight <- max(E(g)$weight)
# min_weight <- min(E(g)$weight)

# E(g)$inv_weight <- (max_weight / E(g)$weight - min_weight / max_weight) / 
#  (max_weight / min_weight - min_weight / max_weight)

E(g)$inv_weight <- 1/E(g)$weight 

# Calculate degree centrality (weighted degree) - No inversion needed
degree_centrality <- strength(g, mode = "all", weights = E(g)$weight)

# Calculate betweenness centrality (requires Min-Max normalized inverse weights)
betweenness_centrality <- betweenness(g, weights = E(g)$inv_weight, normalized = T)

# Calculate closeness centrality (requires Min-Max normalized inverse weights)
closeness_centrality <- closeness(g, weights = E(g)$inv_weight)

# Calculate eigenvector centrality (no inversion needed)
eigenvector_centrality <- eigen_centrality(g, weights = E(g)$weight)$vector

# Combine results into a single dataframe
centrality_df <- data.frame(
  Sub_emotions = V(g)$name,
  Degree_Centrality = degree_centrality,
  Betweenness_Centrality = betweenness_centrality,
  Closeness_Centrality = closeness_centrality,
  Eigenvector_Centrality = eigenvector_centrality
)

centrality_df <- centrality_df %>%
  arrange(desc(Degree_Centrality))

write.xlsx(centrality_df, file = "centrality_gpt.xlsx")
write.xlsx(centrality_df, file = "centrality_lyric split.xlsx")


#### b) Bootstrapping for 95% confidence intervals for centrality measures-----

# Define the bootstrapping function
bootstrap_centrality <- function(graph, edges_df, num_bootstrap = 1000) {
  nodes <- V(graph)$name
  degree_centrality <- matrix(0, nrow = num_bootstrap, ncol = length(nodes))
  betweenness_centrality <- matrix(0, nrow = num_bootstrap, ncol = length(nodes))
  closeness_centrality <- matrix(0, nrow = num_bootstrap, ncol = length(nodes))
  eigenvector_centrality <- matrix(0, nrow = num_bootstrap, ncol = length(nodes))
  
  for (i in 1:num_bootstrap) {
    # Resample edges with replacement
    resampled_edges <- edges_df[sample(nrow(edges_df), replace = TRUE), ]
    g_resampled <- graph_from_data_frame(resampled_edges, directed = FALSE)
    
    # Set weights for resampled graph
    E(g_resampled)$weight <- resampled_edges$n
    
    # Compute Min-Max normalized inverse weights for resampled graph
    max_weight <- max(E(g_resampled)$weight)
    min_weight <- min(E(g_resampled)$weight)
    E(g_resampled)$inv_weight <- (max_weight / E(g_resampled)$weight - min_weight / max_weight) / 
      (max_weight / min_weight - min_weight / max_weight)
    
    # Calculate centrality measures
    degree_values <- strength(g_resampled, mode = "all", weights = E(g_resampled)$weight)  # No inversion needed
    betweenness_values <- betweenness(g_resampled, weights = E(g_resampled)$inv_weight, normalized = T)  # Inversion needed
    closeness_values <- closeness(g_resampled, weights = E(g_resampled)$inv_weight)  # Inversion needed
    eigenvector_values <- eigen_centrality(g_resampled, weights = E(g_resampled)$weight)$vector  # No inversion needed
    
    # Match the order of nodes in the resampled graph to the original graph
    degree_centrality[i, ] <- degree_values[match(nodes, names(degree_values))]
    betweenness_centrality[i, ] <- betweenness_values[match(nodes, names(betweenness_values))]
    closeness_centrality[i, ] <- closeness_values[match(nodes, names(closeness_values))]
    eigenvector_centrality[i, ] <- eigenvector_values[match(nodes, names(eigenvector_values))]
  }
  
  # Calculate mean and 95% confidence intervals
  centrality_df <- data.frame(
    Sub_emotions = nodes,
    Degree_Centrality_Mean = apply(degree_centrality, 2, mean),
    Degree_Centrality_Lower = apply(degree_centrality, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
    Degree_Centrality_Upper = apply(degree_centrality, 2, function(x) quantile(x, 0.975, na.rm = TRUE)),
    Betweenness_Centrality_Mean = apply(betweenness_centrality, 2, mean),
    Betweenness_Centrality_Lower = apply(betweenness_centrality, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
    Betweenness_Centrality_Upper = apply(betweenness_centrality, 2, function(x) quantile(x, 0.975, na.rm = TRUE)),
    Closeness_Centrality_Mean = apply(closeness_centrality, 2, mean),
    Closeness_Centrality_Lower = apply(closeness_centrality, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
    Closeness_Centrality_Upper = apply(closeness_centrality, 2, function(x) quantile(x, 0.975, na.rm = TRUE)),
    Eigenvector_Centrality_Mean = apply(eigenvector_centrality, 2, mean),
    Eigenvector_Centrality_Lower = apply(eigenvector_centrality, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
    Eigenvector_Centrality_Upper = apply(eigenvector_centrality, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
  )
  
  return(centrality_df)
}

# Run the bootstrapping function
set.seed(123)  # Set seed for reproducibility
centrality_intervals <- bootstrap_centrality(g, connect4, num_bootstrap = 1000)

#### c) Identify network clusters----

# a. Edge Betweenness (Girvan-Newman Algorithm) - No inversion needed
ebc <- cluster_edge_betweenness(g, weights = E(g)$weight)
print(membership(ebc))

# b. Fast Greedy Modularity Optimization - No inversion needed
fgc <- cluster_fast_greedy(g, weights = E(g)$weight)
print(membership(fgc))

# c. Walktrap Algorithm - No inversion needed
wc <- cluster_walktrap(g, weights = E(g)$weight)
print(membership(wc))

# d. Infomap Algorithm - No inversion needed
imc <- cluster_infomap(g, e.weights = E(g)$weight)
print(membership(imc))

# e. Louvain Method - No inversion needed
lc <- cluster_louvain(g, weights = E(g)$weight)
print(membership(lc))


# 3 Interval Estimates/Statistical Tests----

dF <- dF %>%
  mutate(singerType = factor(singerType, levels = c("female", "male", "group")))

DF <- DF %>%
  mutate(singerType = factor(singerType, levels = c("female", "male", "group")))

# Generalized function to perform t-test and return the p-value and critical p-value (alpha)
perform_ttest <- function(df, category, variable, group1, group2) {
  formula <- as.formula(paste(variable, "~", category))
  subset_df <- df[df[[category]] %in% c(group1, group2), ]
  t_test_result <- t.test(formula, data = subset_df, var.equal = FALSE)
  n1 <- sum(subset_df[[category]] == group1)
  n2 <- sum(subset_df[[category]] == group2)
  alpha <- ttestEvidence("lindley", n1, n2)[[1]]
  p_value <- t_test_result$p.value
  
  significant <- p_value < alpha
  return(list(p_value = p_value, alpha = alpha, significant = significant))
}

# Function to perform pairwise t-tests
pairwise_ttests <- function(df, category, variable) {
  # List of unique categories
  categories <- unique(df[[category]])
  
  # Create a dataframe to store the results
  results <- data.frame(pair = character(), p_value = numeric(), alpha = numeric(), significant = logical(), stringsAsFactors = FALSE)
  
  # Perform t-test for all pairs of categories
  for (i in 1:(length(categories) - 1)) {
    for (j in (i + 1):length(categories)) {
      group1 <- categories[i]
      group2 <- categories[j]
      test_result <- perform_ttest(df, category, variable, group1, group2)
      pair <- paste(group1, "vs", group2)
      results <- rbind(results, data.frame(pair = pair, p_value = test_result$p_value, alpha = test_result$alpha, significant = test_result$significant))
    }
  }
  
  assign("pairwise_results", results, envir = .GlobalEnv)
  assign("categories", categories, envir = .GlobalEnv)
  return(results)
}

# Function to create symmetric logical matrix based on the significance results
create_logical_matrix <- function() {
  results <- get("pairwise_results", envir = .GlobalEnv)
  categories <- get("categories", envir = .GlobalEnv)
  
  logical_matrix <- matrix(FALSE, nrow = length(categories), ncol = length(categories),
                           dimnames = list(categories, categories))
  
  for (i in 1:nrow(results)) {
    pair <- strsplit(results$pair[i], " vs ")[[1]]
    row_index <- which(categories == pair[1])
    col_index <- which(categories == pair[2])
    logical_matrix[row_index, col_index] <- results$significant[i]
    logical_matrix[col_index, row_index] <- results$significant[i] # Ensure symmetry
  }
  
  diag(logical_matrix) <- NA # Set the diagonal to NA
  
  return(logical_matrix)
}

# Function to compute interval estimates, with the alpha level varying with sample size
interval_estimate <- function(df, category, variable) {
  categories <- unique(df[[category]])
  results <- data.frame(category = character(), mean = numeric(), lower_limit = numeric(), upper_limit = numeric(), stringsAsFactors = FALSE)
  
  for (cat in categories) {
    subset_df <- df[df[[category]] == cat, ]
    n <- nrow(subset_df)
    mean_val <- mean(subset_df[[variable]])
    sd_val <- sd(subset_df[[variable]])
    alpha <- ttestEvidence("lindley", n)[[1]]
    t_critical <- qt(1 - alpha/2, df = n - 1)
    margin_error <- t_critical * sd_val / sqrt(n)
    lower_limit <- mean_val - margin_error
    upper_limit <- mean_val + margin_error
    
    results <- rbind(results, data.frame(category = cat, alpha = alpha, mean = mean_val, lower_limit = lower_limit, upper_limit = upper_limit))
  }
  
  return(results)
}

# Function to compute interval estimates with alpha fixed at 0.05
interval_estimate2 <- function(df, category, variable) {
  categories <- unique(df[[category]])
  results <- data.frame(category = character(), mean = numeric(), lower_limit = numeric(), upper_limit = numeric(), stringsAsFactors = FALSE)
  alpha <- 0.05  # Fix alpha at 0.05
  
  for (cat in categories) {
    subset_df <- df[df[[category]] == cat, ]
    n <- nrow(subset_df)
    mean_val <- mean(subset_df[[variable]])
    sd_val <- sd(subset_df[[variable]])
    t_critical <- qt(1 - alpha / 2, df = n - 1)
    margin_error <- t_critical * sd_val / sqrt(n)
    lower_limit <- mean_val - margin_error
    upper_limit <- mean_val + margin_error
    
    results <- rbind(results, data.frame(category = cat, mean = mean_val, lower_limit = lower_limit, upper_limit = upper_limit))
  }
  
  return(results)
}

### 3.5.1 Overall sentiment score----

#### 3.5.1.1 By singerType, for GPT----------

pairwise_ttest_results <- pairwise_ttests(dF, "singerType", "IntensityWithSign")
logical_matrix <- create_logical_matrix()

interval_results <- interval_estimate(dF, "singerType", "IntensityWithSign")
interval_results2 <- interval_estimate2(dF, "singerType", "IntensityWithSign")

#### 3.5.1.4 By singerType, for lyric split----------

pairwise_ttest_results <- pairwise_ttests(DF, "singerType", "IntensityWithSign")
logical_matrix <- create_logical_matrix()

interval_results <- interval_estimate(DF, "singerType", "IntensityWithSign")
interval_results2 <- interval_estimate2(DF, "singerType", "IntensityWithSign")

# 4. Manual check ----
tp <- read_xlsx("manual check.xlsx")
tp$gpt <- ifelse(tp$IntensityWithSign1 < 0, "N", "P")
tp$lexicon <- ifelse(tp$IntensityWithSign2 < 0, "N", "P")

tp$gpt_r <- tp$Manual_Check == tp$gpt
tp$lexicon_r <- tp$Manual_Check == tp$lexicon

tp$gpt_T_P <- tp$Manual_Check == "P" & tp$gpt == 'P'
tp$gpt_F_P <- tp$Manual_Check == "N" & tp$gpt == 'P'
tp$gpt_T_N <- tp$Manual_Check == "N" & tp$gpt == 'N'
tp$gpt_F_N <- tp$Manual_Check == "P" & tp$gpt == 'N'

tp$lexicon_T_P <- tp$Manual_Check == "P" & tp$lexicon == 'P'
tp$lexicon_F_P <- tp$Manual_Check == "N" & tp$lexicon == 'P'
tp$lexicon_T_N <- tp$Manual_Check == "N" & tp$lexicon == 'N'
tp$lexicon_F_N <- tp$Manual_Check == "P" & tp$lexicon == 'N'


sum(tp$gpt_r)
sum(tp$lexicon_r)

gptTP <- sum(tp$gpt_T_P)
gptFP <- sum(tp$gpt_F_P)
gptTN <- sum(tp$gpt_T_N)
gptFN <- sum(tp$gpt_F_N)

lexiconTP <- sum(tp$lexicon_T_P)
lexiconFP <- sum(tp$lexicon_F_P)
lexiconTN <- sum(tp$lexicon_T_N)
lexiconFN <- sum(tp$lexicon_F_N)

precision_gptP <- gptTP/(gptTP + gptFP)
precision_gptN <- gptTN/(gptTN + gptFN)
precision_gpt_avg <- (precision_gptP + precision_gptN)/2

recall_gptP <- gptTP/(gptTP + gptFN)
recall_gptN <- gptTN/(gptTN + gptFP)
recall_gpt_avg <- (recall_gptP + recall_gptN)/2

f1_gptP <- 2*(precision_gptP * recall_gptP)/(precision_gptP + recall_gptP)
f1_gptN <- 2*(precision_gptN * recall_gptN)/(precision_gptN + recall_gptN)
f1_gpt_avg <- (f1_gptP + f1_gptN)/2

precision_lexiconP <- lexiconTP/(lexiconTP + lexiconFP)
precision_lexiconN <- lexiconTN/(lexiconTN + lexiconFN)
precision_lexicon_avg <- (precision_lexiconP + precision_lexiconN)/2

recall_lexiconP <- lexiconTP/(lexiconTP + lexiconFN)
recall_lexiconN <- lexiconTN/(lexiconTN + lexiconFP)
recall_lexicon_avg <- (recall_lexiconP + recall_lexiconN)/2

f1_lexiconP <- 2*(precision_lexiconP * recall_lexiconP)/(precision_lexiconP + recall_lexiconP)
f1_lexiconN <- 2*(precision_lexiconN * recall_lexiconN)/(precision_lexiconN + recall_lexiconN)
f1_lexicon_avg <- (f1_lexiconP + f1_lexiconN)/2

precision_gptP
precision_gptN 
precision_gpt_avg

recall_gptP
recall_gptN 
recall_gpt_avg 

f1_gptP
f1_gptN 
f1_gpt_avg 

precision_lexiconP 
precision_lexiconN 
precision_lexicon_avg 

recall_lexiconP
recall_lexiconN 
recall_lexicon_avg 

f1_lexiconP 
f1_lexiconN 
f1_lexicon_avg

table("ChatGPT-assisted" = tp$gpt_r, "Lexicon-only" = tp$lexicon_r)
