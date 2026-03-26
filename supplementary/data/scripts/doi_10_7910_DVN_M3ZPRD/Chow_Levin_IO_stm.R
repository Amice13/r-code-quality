###################################################################################
####               Structural Topic Modeling                            ##########
###################################################################################
#load library
library(stm)
library(dplyr)
library(ggplot2)
library(stminsights)
library(ggrepel)

# input data
comments <- read.csv("open.csv") 

# Note: seeds are specified to model as the two models (election and refugee) 
# were analyzed at different time points. Seeds are consistent within model to
# replicability.

##########################################################################################
## Election scenario
  # subset election data
elect <- subset(comments, select = c("id", "open1", "what1", "ccode1"))

  # process text
processed1 <- textProcessor(elect$open1, metadata=elect,  
                           lowercase=TRUE, removestopwords=TRUE, 
                           removepunctuation=TRUE, wordLengths = c(3,Inf),
                           stem=TRUE, language="en", verbose=TRUE,
                           onlycharacter=TRUE)
out1 <- prepDocuments(processed1$documents, processed1$vocab, processed1$meta)

docs1 <- out1$documents
vocab1 <- out1$vocab
meta1 <- out1$meta

  # adjustment to ensure row conformity to find respondent comments
adjust1 <- elect[-processed1$docs.removed,]
clean1 <-adjust1[-out1$docs.removed,]

  # generating topical frequency point estimates
m1 <- stm(documents=docs1, vocab=vocab1, K=9, seed = 2021,
          prevalence = ~ what1+ccode1, data=meta1, 
          init.type="Spectral", max.em.its=200)
prep1 <- estimateEffect(1:9 ~ what1 + ccode1, m1, meta=meta1, 
                        uncertainty="Global")
effects1 <- get_effects(estimates = prep1,
                       variable = 'what1',
                       type = 'pointestimate')

## Refugee scenario
  # subset refugee data
refugee <- subset(comments, select = c("id", "open2", "what2", "ccode2"))

# process text
processed2 <- textProcessor(refugee$open2, metadata=refugee,  
                            lowercase=TRUE, removestopwords=TRUE, 
                            removepunctuation=TRUE, wordLengths = c(3,Inf),
                            stem=TRUE, language="en", verbose=TRUE,
                            onlycharacter=TRUE)

out2 <- prepDocuments(processed2$documents, processed2$vocab, processed2$meta)

docs2 <- out2$documents
vocab2 <- out2$vocab
meta2 <- out2$meta

  # adjustment to ensure row conformity to find respondent comments
adjust2 <- refugee[-processed2$docs.removed,]
clean2 <- adjust2[-out2$docs.removed,]

  # generating topical frequency point estimates
m2 <- stm(documents=docs2, vocab=vocab2, K=8, seed = 9999,
          prevalence = ~ what2+ccode2, data=meta2, 
          init.type="Spectral", max.em.its=200)
prep2 <- estimateEffect(1:8 ~ what2+ccode2, m2, meta=meta2, 
                        uncertainty="Global")
effects2 <- get_effects(estimates = prep2,
                       variable = 'what2',
                       type = 'pointestimate')


##########################################################################################
## Generating Figures and Tables

#############################
### Figure 6 in main text ###
#############################

  # figure 6a (topic 8)
pdf(file = "Figure6a.pdf", width = 5, height = 4)  # saving pdf plot
effects1 %>% filter(topic == 8) %>%
  ggplot(aes(x = value, y = proportion)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.075) +
  geom_point(size = 1.5) + theme_gray() + 
  labs(x = 'Response by the Target of Criticism', y = 'Topic Proportion') + 
  scale_x_discrete(limits = c("No Comment", "Denial", "Irrelevant", 
                              "Past", "Relevant"), 
                   labels = c("Irrelevant" = "Unrelated",
                              "Relevant" = "Recent"))
  dev.off()
  
  # figure 6b (topic 4)
pdf(file = "Figure6b.pdf", width = 5, height = 4)  # saving pdf plot
effects1 %>% filter(topic == 4) %>%
        ggplot(aes(x = value, y = proportion)) +
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.075) +
        geom_point(size = 1.5) + theme_gray() + 
        labs(x = 'Response by the Target of Criticism', y = 'Topic Frequency') + 
        scale_x_discrete(limits = c("No Comment", "Denial", "Irrelevant", 
                              "Past", "Relevant"), 
                         labels = c("Irrelevant" = "Unrelated",
                              "Relevant" = "Recent"))
  dev.off()
  
  # figure 6c (topic 2)
pdf(file = "Figure6c.pdf", width = 5, height = 4)  # saving pdf plot
effects2 %>% filter(topic == 2) %>%
        ggplot(aes(x = value, y = proportion)) +
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.075) +
        geom_point(size = 1.5) + theme_gray() + 
        labs(x = 'Response by the Target of Criticism', y = 'Topic Proportion') + 
        scale_x_discrete(limits = c("No Comment", "Denial", "Irrelevant", 
                                    "Past", "Relevant"), 
                         labels = c("Irrelevant" = "Unrelated",
                                    "Relevant" = "Recent"))
  dev.off()
  
  # figure 6d (topic 5)
pdf(file = "Figure6d.pdf", width = 5, height = 4)  # saving pdf plot
effects2 %>% filter(topic == 5) %>%
        ggplot(aes(x = value, y = proportion)) +
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.075) +
        geom_point(size = 1.5) + theme_gray() + 
        labs(x = 'Response by the Target of Criticism', y = 'Topic Proportion') + 
        scale_x_discrete(limits = c("No Comment", "Denial", "Irrelevant", 
                                    "Past", "Relevant"), 
                         labels = c("Irrelevant" = "Unrelated",
                                    "Relevant" = "Recent"))
  dev.off()


## Representative Comments

## Table A9 
# Comments for Table 1 and Table A8 for election scenario
t4 <- findThoughts(m1, texts=clean1$open1, topic=4, n=10)$docs[[1]] # freq6
t8 <- findThoughts(m1, texts=clean1$open1, topic=8, n=10)$docs[[1]] # freq6

t4
t8

## Table A10
# Comments for Table 1 and Table A9 for refugee scenario
t2 <- findThoughts(m2, texts=clean2$open2, topic=2, n=10)$docs[[1]] # freq4
t5 <- findThoughts(m2, texts=clean2$open2, topic=5, n=10)$docs[[1]] # freq4

t2
t5


################################################################################
## search for optimal K Election data
storage1<-searchK(docs1, vocab1, K = c(5:20), prevalence=~ what1+ccode1, 
                  data=meta1, max.em.its=200, seed=2021, init.type="Spectral")

### Figure A28
## plotting search K for held-out likelihood and residual comparison
pdf(file = "FigureA28.pdf", width = 8, height = 6)  # saving pdf plot
plot(storage1)
dev.off()

################################################################################
## search for optimal K Refugee data
storage2<-searchK(docs2, vocab2, K = c(5:20), prevalence=~ what2+ccode2, 
                  data=meta2, max.em.its=200, seed=9999, init.type="Spectral")

### Figure A30
## plotting search K for held-out likelihood and residual comparison
pdf(file = "FigureA30.pdf", width = 8, height = 6)  # saving pdf plot
plot(storage2)
dev.off()


################################################################################
## Search for optimal K using exclusivity and semantic coherence

### Figure A29
# searching for exclusivity condition
topic.search1 <- manyTopics(documents = docs1,
                            vocab = vocab1,
                            K = 5:12,
                            runs=10,
                            prevalence = ~ what1+ccode1,
                            data = meta1,
                            seed = 2021,
                            max.em.its=200)

exc1 <- 0
for(i in 1:8){
  exc1[i] <- mean(topic.search1$exclusivity[[i]])
}

sem1 <- 0
for(i in 1:8){
  sem1[i] <- mean(topic.search1$semcoh[[i]])
}

id1 <- c(5:12)

dframe1 <- data.frame(id1, exc1, sem1)

exc_plot1 <- ggplot(dframe1, aes(x = exc1, y = sem1)) + geom_point() + 
  xlab("Exclusitivity") + ylab("Semantic Coherence")

pdf(file = "FigureA29.pdf", width = 6, height = 4)  # saving pdf plot
exc_plot1 + geom_text_repel(aes(label = id1), size = 4)
dev.off()


### Figure A31
# searching for exclusivity condition
topic.search2 <- manyTopics(documents = docs2,
                           vocab = vocab2,
                           K = 5:12,
                           runs=10,
                           prevalence = ~ what2+ccode2,
                           data = meta2,
                           seed = 9999,
                           max.em.its=200)

exc2 <- 0
for(i in 1:8){
  exc2[i] <- mean(topic.search2$exclusivity[[i]])
}

sem2 <- 0
for(i in 1:8){
  sem2[i] <- mean(topic.search2$semcoh[[i]])
}

id <- c(5:12)

dframe2 <- data.frame(id, exc2, sem2)

exc_plot2 <- ggplot(dframe2, aes(x = exc2, y = sem2)) + geom_point() + 
                   xlab("Exclusitivity") + ylab("Semantic Coherence")

pdf(file = "FigureA31.pdf", width = 6, height = 4)  # saving pdf plot
exc_plot2 + geom_text_repel(aes(label = id1), size = 4)
dev.off()

