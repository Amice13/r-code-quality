N.topics <- 4
names.left <- c("Values (L)", "Ideologies (L)", "Parties (L)", "Policies (L)")  # mostprobablewords and thoughts
names.right <- c("Right-wing extremists (R)", "Parties (R)", "Ideologies (R)", "Xenophobia (R)")



  # 1. Do different people associate different things with vague concepts?

    # FIGURES WORDCOUNT AND WORDCLOUD
    # WORD COUNT
      left <- as.character(d$left)
      right <- as.character(d$right)
      tdm.left <- dfm(left, ignoredFeatures=tm::stopwords("german"))
      tdm.left <- trim(tdm.left, sparsity=1-0.005)
      tdm.right <- dfm(right, ignoredFeatures=tm::stopwords("german"))
      tdm.right <- trim(tdm.right, sparsity=1-0.005)
    ## adding word count for each individual
      left.word.count <- sapply(gregexpr("\\W+", left), function(x) sum(x>0) )
      right.word.count <- sapply(gregexpr("\\W+", right), function(x) sum(x>0) )

    # FIGURE HISTOGRAMS ON LENGTH OF RESPONSES
      # pdf(file="plots/wordcount_allwords.pdf", width=6, height=3)
      # par(mfrow=c(1,2), mar=c(4,4,1,1))
      # hist(left.word.count, main="", breaks = seq(-.5, 43, 1), xlab="N words for left", ylim=c(0,1500))
      # hist(right.word.count, main="", breaks = seq(-.5, 43, 1), xlab="N words for right", ylim=c(0,1500))
      # dev.off()

  # FIGURE WORDCLOUD
      left.counts <- sort(colSums(as.matrix(tdm.left)), decreasing = TRUE)
      right.counts <- sort(colSums(as.matrix(tdm.right)), decreasing = TRUE)
      left.counts <- data.frame(word = names(left.counts),
                                count_left = as.numeric(left.counts), stringsAsFactors=F)
      right.counts <- data.frame(word = names(right.counts),
                                 count_right = as.numeric(right.counts), stringsAsFactors=F)
      counts <- merge(left.counts, right.counts, all=TRUE)
    ##
      counts$count_left[is.na(counts$count_left)] <- 0
      counts$count_right[is.na(counts$count_right)] <- 0
      counts$ideology <- counts$count_right - counts$count_left
      counts$order <- as.numeric(factor(counts$word))
    # TRANSLATE WORDS
      counts$translation <- counts$word
      counts$translation <- car::recode(counts$translation, "'aber'='but'; 'ausländer'='foreigner'; 'ausländerfeindlich'='xenophobic'; 'ausländerfeindlichkeit'='xenophobia'; 'bürger'='citizen'; 'chancengleichheit'='equality of opportunity'; 'demokratie'='democracy'; 'denken'='think'; 'deutschland'='Germany'; 'diktatur'='dictatorship';
      'einstellung'='attitude'; 'extrem'='extreme'; 'faschismus'='facism'; 'freie'='free';  'freiheit'='freedom'; 'fremdenfeindlichkeit'='xenophobia'; 'ganz'='quite'; 'gedankengut'='ideas'; 'gegenteil'='contrary'; 'gerecht'='just'; 'gerechtigkeit'='justice';
      'gewalt'='violence'; 'gleichberechtigung'='equal rights'; 'gleiche'='equal'; 'gleichheit'='equality'; 'gleichmacherei'='leveling'; 'grüne'='greens'; 'grünen'='green party'; 'gut'='good'; 'immer'='always'; 'interessen'='interests'; 'intoleranz'='intolerance'; 'kapitalismus'='capitalism'; 'kleinen'='the small';
      'kommunismus'='communism'; 'kommunisten'='communists'; 'konservativ'='conservative'; 'konservative'='the conservatives'; 'leute'='people'; 'linke'='the left'; 'linken'='leftists'; 'links'='left'; 'linkspartei'='left party'; 'meinung'='opinion'; 'menschen'='humans'; 'national'='national'; 'nationale'='nationalist'; 'nationalismus'='nationalism'; 'nationalistisch'='nationalistic'; 'nationalsozialismus'='national socialism';
      'ordnung'='order'; 'partei'='party'; 'parteien'='parties'; 'politik'='politics'; 'radikal'='radical'; 'radikale'='radicals'; 'radikalismus'='radicalism'; 'rassismus'='racism'; 'recht'='right'; 'rechte'='the right'; 'rechten'='rightists'; 'rechts'='on the right'; 'rechtsradikal'='right-wing extremist'; 'rechtsradikale'='right-wing extremist'; 'rechtsradikalen'='right-wing extremist'; 'rechtsradikalismus'='right-wing radicalism'; 'reich'='rich'; 'richtung'='direction';
      'sozial'='social'; 'soziale'='social'; 'sozialen'='social'; 'sozialer'='more social'; 'soziales'='social'; 'sozialismus'='socialism'; 'tun'='do'; 'umverteilung'='redistribution'; 'unterdrückung'='oppression'; 'verbinde'='associate'; 'vertreten'='represent'; 'wirtschaft'='economy';
      'grünere'='greener';'bürger'='citizen';'grünen'='green';'dass'='that';'unterdrückung'='oppression';'eher'='rather';'mehr'='more'")
    # WORDCLOUD GRAPH GERMAN
      library(wordcloud)
      mat <- counts[,c("count_left", "count_right")]
      names(mat) <- c('Words that are associated with "left"', 'Words that are associated with "right"')
      dimnames(mat)[[1]] <- counts$word
      pdf("plots/wordcloud.pdf", height=4, width=5)
      comparison.cloud(mat, max.words=70, scale=c(2.5, .5), rot.per=0,
                     colors=c("black", "grey60"), title.size=1, random.order=TRUE)
      dev.off()
    # WORDCLOUD GRAPH ENGLISH
      library(wordcloud)
      # aggregating rows with same name
      counts$count_left[counts$translation=="social"][1] <- sum(counts$count_left[counts$translation=="social"])
      counts$count_right[counts$translation=="social"][1] <- sum(counts$count_right[counts$translation=="social"])
      counts$count_left[counts$translation=="extreme"][1] <- sum(counts$count_left[counts$translation=="extreme"])
      counts$count_right[counts$translation=="extreme"][1] <- sum(counts$count_right[counts$translation=="extreme"])
      counts$count_left[counts$translation=="right-wing extremist"][1] <- sum(counts$count_left[counts$translation=="right-wing extremist"])
      counts$count_right[counts$translation=="right-wing extremist"][1] <- sum(counts$count_right[counts$translation=="right-wing extremist"])
      counts$count_left[counts$translation=="xenophobia"][1] <- sum(counts$count_left[counts$translation=="xenophobia"])
      counts$count_right[counts$translation=="xenophobia"][1] <- sum(counts$count_right[counts$translation=="xenophobia"])

      counts <- counts[!duplicated(counts$translation),]

      mat <- counts[,c("count_left", "count_right")]
      names(mat) <- c('Words that are associated with "left"', 'Words that are associated with "right"')
      dimnames(mat)[[1]] <- counts$translation
      pdf("plots/wordcloud_translation.pdf", height=4, width=5)
      comparison.cloud(mat, max.words=70, scale=c(2.7, .7), rot.per=0,
                       colors=c("black", "grey60"), title.size=1, random.order=TRUE)
      dev.off()


    # BAR PLOT: NUMBER OF PEOPLE IN DIFFERENT CATEGORIES
      pdf(file="plots/topic_count.pdf", width=10, height=2.5)
      # windows(10,2)
      par(mfrow=c(1,2), mar=c(4,8,2,2))
      x.left <- table(d$max.left)
       names(x.left) <- names.left # see  above
      x.left <- sort(x.left, decreasing=FALSE)
      barplot(x.left,las=2, horiz=TRUE, names.arg=names(x.left), xlim=c(0,900), cex.names=0.8, xlab="N in topic", main="Associations with left", cex.main=.8)
      # text(450, 3.2, "Associations \n with left", col="black", font=2)
      x.right <- table(d$max.right)
      names(x.right) <- names.right # see above
      x.right <- sort(x.right, decreasing=FALSE)
      barplot(x.right,las=2, horiz=TRUE, names.arg=names(x.right), xlim=c(0,900), cex.names=0.8, xlab="N in topic", main="Associations with right", cex.main=.8)
      # text(450, 3.2, "Associations \n with right", col="black", font=2)
      dev.off()

