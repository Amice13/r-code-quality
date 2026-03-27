## run after "01-clean-data.r"

#####################################
## EXTRACT TOPICS: LEFT AND RIGHT
#####################################
 
##### LEFT
    # Processing of text and saving documents (in STM format), 
    # vocabulary (words), and original text in different objects
    processed <- textProcessor(d$left, sparselevel=.999,
                               stem=FALSE, verbose=TRUE)

    out <- prepDocuments(processed$documents, processed$vocab, processed$meta) 
    docs <- out$documents
    vocab <- out$vocab
    texts <- d$left[as.numeric(names(processed$documents))]

    # Model Search
    N.topics <- 4 # IMPORTANT NUMBER FOR BELOW
    ptm <- proc.time()
    modelSelect.left <- selectModel(docs,vocab,K=N.topics, max.em.its=100,
                                    runs=50,seed=12345, LDAbeta=FALSE,
                                    sigma.prior=1) # find a model with good properties (estimation)
    proc.time() - ptm
    plotModels(modelSelect.left)  # plot models to check for cohesivenss and exclusiveness

    # RESULTS
    # 1) top scoring words
    (left.PrevFit <- modelSelect.left$runout[[4]]) # choose best candidate models
    (mostprobablewords.left <- labelTopics(left.PrevFit, n=10))
    # NAME THE TOPICS (-> mostprobablewords, thoughts) !!!
    names.left <- c("Values (L)", "Ideologies (L)", "Parties (L)", "Policies (L)")  # mostprobablewords and thoughts
    pdf(file="plots/words_left.pdf", width=8, height=7); plot(left.PrevFit,type="labels"); dev.off()
    # 2) most representative documents
    thoughts.left <- findThoughts(left.PrevFit, texts, n=4)
    # 3) topic proportions (average)
    topicproportion.left <- apply(left.PrevFit$theta, 2, mean)
    ## Extracts topic probabilities for each individual + puts them into the dataframe
    ## NOTE: missing values for individuals who did not respond!
    pred.prob <- matrix(NA, ncol=N.topics, nrow=nrow(d))
    pred.prob[as.numeric(names(processed$documents)),] <- left.PrevFit$theta
    pred.prob <- data.frame(pred.prob)
    names(pred.prob) <- paste("Ltopic", seq(1,N.topics), sep="")
    d <- cbind(d, pred.prob)
    ## preding top topic for each individual
    pred.topics <- apply(pred.prob, 1, which.max)
    pred.topics[unlist(lapply(pred.topics, function(x) length(x)==0))] <- NA
    d$max.left <- unlist(pred.topics)
    ## this is the distribution of maximum probabilities
    maxs <- apply(pred.prob, 1, max)
    hist(maxs)



###### RIGHT
    
    processed <- textProcessor(d$right, sparselevel=.999, stem=FALSE, verbose=TRUE)
    out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
    docs <- out$documents
    vocab <- out$vocab
    texts <- d$right[as.numeric(names(processed$documents))]
    # Model Search
    N.topics <- 4 # IMPORTANT NUMBER FOR BELOW
    # FIND A GOOD MODEL
    ptm <- proc.time()

    modelSelect.right <- selectModel(docs,vocab,K=N.topics, max.em.its=200,
                                    runs=50,seed=12345, LDAbeta=FALSE,
                                    sigma.prior=1) # find a model with good properties (estimation)
    proc.time() - ptm
if (bauer) windows()
    plotModels(modelSelect.left)  # plot models to check for cohesivenss and exclusiveness

    # RESULTS
    # 1) top scoring words
    (right.PrevFit <- modelSelect.right$runout[[3]]) # choose best candidate models
    (mostprobablewords.right <- labelTopics(right.PrevFit, n=10))
    # NAME THE TOPICS (-> mostprobablewords, thoughts) !!!
    names.right <- c("Right-wing extremists (R)", "Parties (R)", "Ideologies (R)", "Xenophobia (R)")
    pdf(file="plots/words_right.pdf", width=8, height=7); plot(right.PrevFit,type="labels"); dev.off()
    # 2) most representative documents
    thoughts.right <- findThoughts(right.PrevFit, texts, n=5)
    # 3) topic proportions (average)
    topicproportion.right <- apply(right.PrevFit$theta, 2, mean)
    ## Extracts topic probabilities for each individual + puts them into the dataframe
    ## NOTE: missing values for individuals who did not respond!
    pred.prob <- matrix(NA, ncol=N.topics, nrow=nrow(d))
    pred.prob[as.numeric(names(processed$documents)),] <- right.PrevFit$theta
    pred.prob <- data.frame(pred.prob)
    names(pred.prob) <- paste("Rtopic", seq(1,N.topics), sep="")
    d <- cbind(d, pred.prob)
    ## preding top topic for each individual
    pred.topics <- apply(pred.prob, 1, which.max)
    pred.topics[unlist(lapply(pred.topics, function(x) length(x)==0))] <- NA
    d$max.right <- unlist(pred.topics)
    ## this is the distribution of maximum probabilities
    maxs <- apply(pred.prob, 1, max)
    hist(maxs)


    # GENERATE FACTORS FOR TOPICS
    d$max.left.factor <- as.factor(d$max.left)
    d$max.right.factor <- as.factor(d$max.right)


    # SAVE DATASET
    if (!cache) write.csv(d, file = "cache/d.csv", row.names = FALSE)


