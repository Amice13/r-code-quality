#### stm script                        
#load libraries
library(stm)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(stminsights)

# note: working directory defaults to location of extracted files
protest1 <- read.csv("open_concede.csv")  ## input .csv response file 1

### Text processing of concession open responses
processed1 <- textProcessor(protest1$open, metadata=protest1,  
                            lowercase=TRUE, removestopwords=TRUE, 
                            removepunctuation=TRUE, wordLengths = c(3,Inf),
                            stem=TRUE, language="en", verbose=TRUE,
                            onlycharacter=TRUE)
out1 <- prepDocuments(processed1$documents, processed1$vocab, processed1$meta)

docs1 <- out1$documents
vocab1 <- out1$vocab
meta1 <- out1$meta

adjust1<-protest1[-processed1$docs.removed,]  ## need to use clean data for 
clean1<-adjust1[-out1$docs.removed,]   ## generating relevant quotes

### Text processing of repression open responses
protest2 <- read.csv("open_repress.csv")  ## input .csv response file 2
processed2 <- textProcessor(protest2$open, metadata=protest2,  
                            lowercase=TRUE, removestopwords=TRUE, 
                            removepunctuation=TRUE, wordLengths = c(3,Inf),
                            stem=TRUE, language="en", verbose=TRUE,
                            onlycharacter=TRUE)

out2 <- prepDocuments(processed2$documents, processed2$vocab, processed2$meta)

docs2 <- out2$documents
vocab2 <- out2$vocab
meta2 <- out2$meta

adjust2<-protest2[-processed2$docs.removed,]  ## need to use clean data for 
clean2<-adjust2[-out2$docs.removed,]   ## generating relevant quotes

## concede open responses
concede <- stm(documents=docs1, vocab=vocab1, K=20, max.em.its=100, 
               seed = 2021, prevalence=~intervene, data=meta1, 
               init.type="Spectral")

concede1 <- estimateEffect(1:20 ~ intervene, concede, meta=meta1, 
                        uncertainty="Global")

## repress open responses
repress <- stm(documents=docs2, vocab=vocab2, K=20, max.em.its=100, 
               seed = 2021, prevalence=~intervene, data=meta2, 
               init.type="Spectral")

repress1 <- estimateEffect(1:20 ~ intervene, repress, meta=meta2, 
                        uncertainty="Global")


## Plotting Figure 5 in main text
pdf(file = "Figure5.pdf", width = 8.5, height = 4)  # saving pdf plot
par(mfrow=c(1,2), col="black",lwd=1)
plot(concede1, covariate="intervene", topics=c(5,9,12), model=concede, 
     method="difference", cov.value1="0", cov.value2="1",
     xlab = "Change in topical prevalence from interference 
             to no interference", xlim=c(-.07,.07), main="(a) Concede",
     labeltype = "custom", 
     custom.labels = c("Procedural Legitimacy", 
                       "Nationalism & Energy Security","Protester Commitment"))
plot(repress1, covariate="intervene", topics=c(6,7,18), model=repress, 
     method="difference", cov.value1="0", cov.value2="1",
     xlab = "Change in topical prevalence from interference 
            to no interference", xlim=c(-.05,.05), main="(b) Repress",
     labeltype = "custom", custom.labels = c("Environmental Protection",
                                             "Treason","Right to Protest"))
dev.off()



## List out text most associated with relevant topics
concede5 <- findThoughts(concede, texts=clean1$open, 
                         topic=5, n=10)$docs[[1]] 
concede9 <- findThoughts(concede, texts=clean1$open, 
                         topic=9, n=10)$docs[[1]]
concede12 <- findThoughts(concede, texts=clean1$open, 
                          topic=12, n=10)$docs[[1]]

repress6 <- findThoughts(repress, texts=clean2$open, 
                         topic=6, n=10)$docs[[1]] 
repress7 <- findThoughts(repress, texts=clean2$open, 
                         topic=7, n=10)$docs[[1]] 
repress18 <- findThoughts(repress, texts=clean2$open, 
                          topic=18, n=10)$docs[[1]] 

## Display top 10 comments of which 2 were selected in the main text
concede5  ## Procedural legitimacy (6%)
concede9  ## Nationalism/energy security (5%)
concede12 ## Protester commitment (5%)

repress6  ## Environmental Protection (6%)
repress7  ## Treason (6%)
repress18 ## Right to Protest (13%)

################################################################################
## Figure A7.1 in appendix: Comparing open responses to topic selections

protest1a <- protest1[complete.cases(protest1),]

### Text processing of concession open responses
processed1a <- textProcessor(protest1a$open, metadata=protest1a,  
                           lowercase=TRUE, removestopwords=TRUE, 
                           removepunctuation=TRUE, wordLengths = c(3,Inf),
                           stem=TRUE, language="en", verbose=TRUE,
                           onlycharacter=TRUE)

out1a <- prepDocuments(processed1a$documents, processed1a$vocab, processed1a$meta)

docs1a <- out1a$documents
vocab1a <- out1a$vocab
meta1a <- out1a$meta

## concede open responses
concede <- stm(documents=docs1a, vocab=vocab1a, K=20, max.em.its=100, 
                 seed = 2021, prevalence=~intervene+natsec+commit, data=meta1a, 
                 init.type="Spectral")

prep <- estimateEffect(1:20 ~ intervene + natsec + commit, concede, 
                       meta=meta1a, uncertainty="Global")

effects1 <- get_effects(estimates = prep,
                        variable = 'natsec',
                        type = 'pointestimate')

effects2 <- get_effects(estimates = prep,
                        variable = 'commit',
                        type = 'pointestimate')

# Figure A7.1a
pdf(file = "Figure7.1a.pdf", width = 5, height = 3.5)  # saving pdf plot
effects1 %>% filter(topic == 9) %>%
        ggplot(aes(x = value, y = proportion)) +
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.075, size = .5) +
        geom_point(size = 1.5) + theme_gray() + labs(x = 'Importance of Energy Security', 
                                               y = 'Topic Proportion') +
        theme(text = element_text(size=12))
dev.off()

# Figure A7.1b
pdf(file = "Figure7.1b.pdf", width = 5, height = 3.5)  # saving pdf plot
effects2 %>% filter(topic == 12) %>%
        ggplot(aes(x = value, y = proportion)) +
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.075, size = .5) +
        geom_point(size = 1.5) + theme_gray() + labs(x = 'Protester Commitment', 
                                                     y = 'Topic Proportion') +
        theme(text = element_text(size=12))
dev.off()

### Table A7.2
summary(prep, topic=9) ## column 1 (energy security) for Table A7.2
summary(prep, topic=12) ## column 2 (protester commitment) for Table A7.2

### Table A7.3

summary(concede1, topic=5) ## column 1 (Procedural Legitimacy) for Table A7.3
summary(concede1, topic=9) ## column 2 (Nationalism & Energy Security) for Table A7.3
summary(concede1, topic=12) ## column 3 (Protester Commitment) for Table A7.3
summary(repress1, topic=6) ## column 2 (Environmental Protection) for Table A7.3
summary(repress1, topic=7) ## column 2 (Treason) for Table A7.3
summary(repress1, topic=18) ## column 2 (Right to Protest) for Table A7.3


### Figure A7.2 "Concede"
concede_k <- searchK(docs1, vocab1, K = c(10, 20, 30, 40, 50, 60), seed = 2021,
                   prevalence =~ intervene, data = meta1, max.em.its=100)

# Plot Figure A7.2 (top)
pdf(file = "Figure7.2top.pdf", width = 8, height = 6.5)  # saving pdf plot
plot(concede_k) # note: we only analyzed top-half of diagnostic statistics
                # focusing on held-out likelihood and residuals
dev.off()

### Figure A7.2 "Repress"
repress_k <- searchK(docs2, vocab2, K = c(5, 10, 20, 30, 40, 50, 60), seed = 2021,
                     prevalence =~ intervene, data = meta2, max.em.its=100)

# Plot Figure A7.2 (bottom)
pdf(file = "Figure7.2bottom.pdf", width = 8, height = 6.5)  # saving pdf plot
plot(repress_k) # note: we only analyzed top-half of diagnostic statistics
                # focusing on held-out likelihood and residuals
dev.off()


### Figure A7.3
# Figure A7.3a: Concede Topics

  ## search for optimal K

topic.search <- manyTopics(documents = docs1,
                           vocab = vocab1,
                           K = c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30),
                           runs=10,
                           prevalence = ~ intervene,
                           data = meta1,
                           seed = 2021,
                           max.em.its=100)

exc <- 0
for(i in 1:11){
  exc[i] <- mean(topic.search$exclusivity[[i]])
}

sem <- 0
for(i in 1:11){
  sem[i] <- mean(topic.search$semcoh[[i]])
}

id <- c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30)

dframe <- data.frame(id, exc, sem)


## plot the figure
exc_plot <- ggplot(dframe, aes(x = sem, y = exc)) + geom_point() + 
            xlab("Semantic Coherence") + ylab("Exclusitivity")
                  

# generate spacing in figure
pdf(file = "Figure7.3a.pdf", width = 5, height = 3.5)  # saving pdf plot
exc_plot + geom_text_repel(aes(label = id), size = 4)
dev.off()


### Figure A7.3b: Repress Topic Search

## search for optimal K

topic.search1 <- manyTopics(documents = docs2,
                             vocab = vocab2,
                             K = c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30),
                             runs=10,
                             prevalence = ~ intervene,
                             data = meta2,
                             seed = 2021,
                             max.em.its=100)


exc1 <- 0
for(i in 1:11){
  exc1[i] <- mean(topic.search1$exclusivity[[i]])
}

sem1 <- 0
for(i in 1:11){
  sem1[i] <- mean(topic.search1$semcoh[[i]])
}

id1 <- c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30)

dframe1 <- data.frame(id1, exc1, sem1)


## plot the figure
exc_plot1 <- ggplot(dframe1, aes(x = sem1, y = exc1)) + 
                    geom_point() + xlab("Semantic Coherence") + 
                    ylab("Exclusitivity")


# generate spacing in figure
pdf(file = "Figure7.3b.pdf", width = 5, height = 3.5)  # saving pdf plot
exc_plot1 + geom_text_repel(aes(label = id1), size = 4)
dev.off()