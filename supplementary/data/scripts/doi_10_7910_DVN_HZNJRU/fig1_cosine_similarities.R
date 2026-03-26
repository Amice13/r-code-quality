## Reproduce Figure 1 in Montero-Melis et al. (2019, Cognition)

library("tidyverse")

getwd()  # make sure you are in the path where the data file is located

# load data
cosines <- read_csv("exp2_stimuli_with_cosine_sub2vec.csv")
head(cosines)


# For the plot, create labels for points in the upper / lower extremes
cosines$labels_s2v <- cosines$word_pair
by(cosines$cosine_s2v, cosines$match_type, sort)  # explore
# Remove all labels except extreme points
cosines$labels_s2v[with(cosines,
                        match_type == "category" &
                          ! (cosine_s2v <= .325 | cosine_s2v > .76) )] <- NA
cosines$labels_s2v[with(cosines,
                        match_type == "shape" &
                          ! (cosine_s2v <= .15 | cosine_s2v > .4) )] <- NA

# Plot
set.seed(1322231)  # make jittering of points reproducible
ggplot(cosines, aes(x = match_type, y = cosine_s2v, label = labels_s2v)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "red",
               width = .2, size = 2) +
  geom_jitter(height = 0, width = .2, alpha = .2, size = 3) +
  geom_text() +
  xlab("Type of match") +
  ylab("Cosine similarity") +
  theme_classic()

# save to disk
ggsave("fig_sub2vec.tiff", height = 4, width = 5, units = 'in', dpi = 300)
