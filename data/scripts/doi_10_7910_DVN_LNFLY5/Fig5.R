# this was run on R version 4.4.1
# load packages
library(tm)
library(stm)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(tidytext)
library(tidyr)
library(forcats)
library(purrr)
library(scales)
library(MetBrewer)
library(ggpubr)

# ggplot theme 
theme_mfx <- function() {
  theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey80", color = NA))
}

# Johnson color palette
clrs <- met.brewer("Johnson",7)

# ===================================================================================#
#
# BENEFITS OF AI
#
#====================================================================================#

#====================================================================================#
#
# Pre-processing
#
#====================================================================================#

# data
dat_text <- read.csv("dat_benefits_text.csv") 

# remove apostrophes (extra check)
dat_text$benefits_text <- str_replace_all(dat_text$benefits_text, "’", "")

# other preprocessing
processed <- stm::textProcessor(
  dat_text$benefits_text, 
  metadata = dat_text,
  lowercase = TRUE,
  removestopwords = TRUE,
  removenumbers = TRUE,
  removepunctuation = TRUE,
  ucp = FALSE, 
  stem = TRUE,
  language = "en",
  customstopwords = NULL,
  custompunctuation = NULL
)

# view vocab
processed$vocab
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

#====================================================================================#
#
# Figure S12: STM model (Benefits)
#
#====================================================================================#

# STM Model, K = 15
mod <- stm::stm(documents = out$documents, 
                vocab = out$vocab, 
                K=15, prevalence=~gender_bin, 
                data = out$meta, 
                seed=123, 
                init.type = "Spectral")

# label topics
labelTopics(mod, c(1:15), n=20)

# tidy model
modt <- tidy(mod)

# get highest probability words by betas 
# betas: estimated probability of each word given the topic
top_terms <- modt %>%
  dplyr::arrange(beta) %>%
  dplyr::group_by(topic) %>%
  slice_max(beta, n = 7) %>%
  dplyr::arrange(-beta) %>%
  dplyr::select(topic, term) %>%
  dplyr::summarize(terms = list(term)) %>%
  dplyr::mutate(terms = map(terms, paste, collapse = ", ")) 

# get document–topic proportions
# gamma: estimated probability each document is generated from each topic
modt_gamma <- tidy(mod, 
                   matrix = "gamma")

# get topic-level summary:
# each topic's average prevalence in corpus + "top terms" label for each topic.

# top words contributing to each topic 
gamma_terms <- modt_gamma %>%
  dplyr::group_by(topic) %>%
  dplyr::summarize(gamma = mean(gamma), .groups = "drop") %>%
  dplyr::arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  dplyr::mutate(topic = paste0("Topic ", topic))

# label topics
gamma_terms <-
  gamma_terms %>%
  mutate(topic_label = case_when(
    topic == "Topic 1"  ~ "productivity and efficiency", 
    topic == "Topic 2"  ~ "speed-up processes",
    topic == "Topic 3"  ~ "dangerous/no benefits", 
    topic == "Topic 4"  ~ "automate mundane tasks",
    topic == "Topic 5"  ~ "streamline processes",
    topic == "Topic 6"  ~ "science and medical advances",
    topic == "Topic 7"  ~ "efficiency and cost savings",
    topic == "Topic 8"  ~ "make life easier",
    topic == "Topic 9"  ~ "not sure/no benefits",
    topic == "Topic 10" ~ "various benefits", # patient care
    topic == "Topic 11" ~ "don't know",
    topic == "Topic 12" ~ "no benefits",
    topic == "Topic 13" ~ "better analysis/decisions",
    topic == "Topic 14" ~ "better problem-solving",
    topic == "Topic 15" ~ "reduces human error"
  ))

# get ordering and save 
top_gamma_terms <- gamma_terms %>%
  slice_max(gamma, n = 15, with_ties = FALSE) %>%
  dplyr::arrange(desc(gamma)) 

top_topics <- top_gamma_terms$topic_label

# re-level the topic column based on the order of top_topics
gamma_terms <- gamma_terms %>%
  dplyr::mutate(
    topic_label = factor(
    topic_label, levels = top_topics)
    )  

# select 15 colors
c15 <- met.brewer("Johnson",25)[11:25]
# reverse
c15 <- rev(c15)

# dat for plot
gamma_terms_plot <- gamma_terms %>%
  slice_max(gamma, n = 15, with_ties = FALSE) %>%
  mutate(topic_label = factor(topic_label, levels = rev(top_topics)))

# plot
ggplot(gamma_terms_plot, aes(topic_label, gamma, label = terms, fill = topic_label)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.35), labels = percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme_mfx() +
  scale_fill_manual(values = c15) +
  labs(x = NULL, y = "Average topic prevalence")
ggsave(filename = "Figures/textanalysis_terms.png", width = 6.6, height = 3.6, dpi = 350)


#====================================================================================#
#
# Figure 5: Marginal Effect of Gender (Benefits)
#
#====================================================================================#

# estimate differences in topic prevalence by gender for topics 1-15
out$meta$gender_bin <- factor(out$meta$gender_bin, labels = c("Man", "Woman"))

mod_diff <- estimateEffect(1:15 ~ gender_bin, 
                       mod, meta = out$meta, 
                       uncertainty = "Global")

# get topic IDs
top_topics_num <- top_gamma_terms$topic
topic_numbers <- as.numeric(gsub("Topic ", "", top_topics_num))

# tidy for plot
mod_difft <- tidy(mod_diff, conf.int=T)
mod_difft <- mod_difft %>% filter(term != "(Intercept)",
                          topic %in% c(topic_numbers))

# merge with topic names
mod_difft$topic_num <- mod_difft$topic
mod_difft <- mod_difft %>%
  dplyr::mutate(topic = paste0("Topic ", topic)) %>%
  left_join(gamma_terms %>% select(topic, topic_label), by = "topic")

# plot
gender_benefits <- ggplot(mod_difft, aes(x = estimate, y = topic_num)) +
  geom_vline(xintercept = 0, linewidth = 0.5, linetype = "24", color = clrs[7]) +
  geom_pointrange(aes(xmin = estimate - 1.96*std.error, 
                      xmax = estimate + 1.96*std.error), color = clrs[7],
                  size = 0.25) +
  geom_text(aes(label = topic_label), vjust=-0.4, 
            color = clrs[7],
            size = 2.5) +
  theme_mfx() +
  xlim(-0.055, 0.1) +
  ggtitle("Marginal Effect of Gender on Topic Prevalence (Benefits)") +
  labs(x = "", y = "") +  
  theme(axis.text.y = element_blank(),
        plot.title = element_text(size = 8.5)
  ) 
ggsave(gender_benefits, filename = "Figures/textanalysis_gender.png", width = 6.6, height = 3.6, dpi = 350)



# ===================================================================================#
#
# RISKS
#
#====================================================================================#


#====================================================================================#
#
# Data Prep: Risks of AI 
#
#====================================================================================#

# data
dat_text <- read.csv("dat_risks_text.csv") # translated data

# remove apostrophes (extra check)
dat_text$risks_text <- str_replace_all(dat_text$risks_text, "’", "")

# other preprocessing
processed <- stm::textProcessor(
  dat_text$risks_text, 
  metadata = dat_text,
  lowercase = TRUE,
  removestopwords = TRUE,
  removenumbers = TRUE,
  removepunctuation = TRUE,
  ucp = FALSE, #removes more general punctuation - default is F
  stem = TRUE,
  language = "en",
  customstopwords = NULL,
  custompunctuation = NULL
)

# view vocab
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

#====================================================================================#
#
# Figure S13: STM model (Risks)
#
#====================================================================================#

# STM Model, K = 15
mod <- stm(documents = out$documents, 
           vocab = out$vocab, 
           K=15, prevalence=~gender_bin, 
           data = out$meta, 
           seed=123, 
           init.type = "Spectral")

# label topics
labelTopics(mod, c(1:15), n = 25)

# tidy model
modt <- tidy(mod)

# get highest probability words by betas 
# betas: estimated probability of each word given the topic
top_terms <- modt %>%
  dplyr::arrange(beta) %>%
  dplyr::group_by(topic) %>%
  slice_max(beta, n = 7) %>%
  dplyr::arrange(-beta) %>%
  dplyr::select(topic, term) %>%
  dplyr::summarize(terms = list(term)) %>%
  dplyr::mutate(terms = map(terms, paste, collapse = ", "))

# get document–topic proportions
# gamma: estimated probability each document is generated from each topic
modt_gamma <- tidy(mod, matrix = "gamma")

# get topic-level summary:
# each topic's average prevalence in corpus + "top terms" label for each topic.

# top words contributing to each topic 
gamma_terms <- modt_gamma %>%
  dplyr::group_by(topic) %>%
  dplyr::summarize(gamma = mean(gamma), .groups = "drop") %>%
  dplyr::arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  dplyr::mutate(topic = paste0("Topic ", topic))

# label topics
gamma_terms <-
  gamma_terms %>%
  mutate(topic_label = case_when(
    topic == "Topic 1" ~ "humans become dependent", #degrades human capacity
    topic == "Topic 2" ~ "AI becomes sentient",
    topic == "Topic 3" ~ "mis/disinformation",
    topic == "Topic 4" ~ "humans lose control",
    topic == "Topic 5" ~ "privacy and data bias", 
    topic == "Topic 6" ~ "mass job loss",
    topic == "Topic 7" ~ "don't know",
    topic == "Topic 8" ~ "nefarious and illegal uses",
    topic == "Topic 9" ~ "manipulates humans",
    topic == "Topic 10" ~ "takes jobs",
    topic == "Topic 11" ~ "loss of human element",
    topic == "Topic 12" ~ "economic disparities",
    topic == "Topic 13" ~ "overreliance",
    topic == "Topic 14" ~ "loss of human interaction",
    topic == "Topic 15" ~ "loss of skills/jobs" 
  ))

# get ordering and save 
top_gamma_terms <- gamma_terms %>%
  slice_max(gamma, n = 15, with_ties = FALSE) %>%
  dplyr::arrange(desc(gamma)) 

top_topics <- top_gamma_terms$topic_label

# re-level the topic column based on the order of top_topics
gamma_terms <- gamma_terms %>%
  dplyr::mutate(
    topic_label = factor(
      topic_label, levels = top_topics)
  )  

# select 15 colors
c15 <- met.brewer("Johnson",25)[11:25]
# reverse
c15 <- rev(c15)

# dat for plot
gamma_terms_plot <- gamma_terms %>%
  slice_max(gamma, n = 15, with_ties = FALSE) %>%
  mutate(topic_label = factor(topic_label, levels = rev(top_topics)))

# plot
ggplot(gamma_terms_plot, aes(topic_label, gamma, label = terms, fill = topic_label)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.35), labels = percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme_mfx() +
  scale_fill_manual(values = c15) +
  labs(x = NULL, y = "Average topic prevalence")
ggsave(filename = "Figures/textanalysis_terms_risks.png", width = 6.6, height = 3.6, dpi = 350)


#====================================================================================#
#
# Figure 5: Marginal Effect of Gender (Risks)
#
#====================================================================================#

# estimate differences in topic prevalence by gender for topics 1-15
out$meta$gender_bin <- factor(out$meta$gender_bin, labels = c("Man", "Woman"))

mod_diff <- estimateEffect(1:15 ~ gender_bin, 
                           mod, meta = out$meta, 
                           uncertainty = "Global")

# get topic IDs
top_topics_num <- top_gamma_terms$topic
topic_numbers <- as.numeric(gsub("Topic ", "", top_topics_num))

# tidy for plot
mod_difft <- tidy(mod_diff, conf.int=T)
mod_difft <- mod_difft %>% filter(term != "(Intercept)",
                          topic %in% c(topic_numbers))

# merge with topic names
mod_difft$topic_num <- mod_difft$topic
mod_difft <- mod_difft %>%
  dplyr::mutate(topic = paste0("Topic ", topic)) %>%
  left_join(gamma_terms %>% select(topic, topic_label), by = "topic")

# plot
gender_risks <- ggplot(mod_difft, aes(x = estimate, y = topic_num)) +
  geom_vline(xintercept = 0, linewidth = 0.5, linetype = "24", color = clrs[7]) +
  geom_pointrange(aes(xmin = estimate - 1.96*std.error, 
                      xmax = estimate + 1.96*std.error), color = clrs[7],
                  size = 0.25) +
  geom_text(aes(label = topic_label), vjust=-0.4, 
            color = clrs[7],
            size = 2.5) +
  theme_mfx() +
  xlim(-0.055, 0.1) +
  ggtitle("Marginal Effect of Gender on Topic Prevalence (Risks)") +
  labs(x = "", y = "") +  
  theme(axis.text.y = element_blank(),
        plot.title = element_text(size = 8.5)
  ) 
ggsave(gender_risks, filename = "Figures/textanalysis_gender_risks.png", width = 6.6, height = 3.6, dpi = 350)

# save combined with benefits
combined <- ggpubr::ggarrange(gender_benefits, gender_risks) 
fig_annotated <- annotate_figure(
  combined,
  left = ggpubr::text_grob("Topic", rot = 90, size = 9),   # rotation = 90° for vertical
  bottom = ggpubr::text_grob("More Man ... More Woman", size = 9)
)

ggsave("Figures/Figure5.png", height=3.9, width=7.3, dpi = 350)

