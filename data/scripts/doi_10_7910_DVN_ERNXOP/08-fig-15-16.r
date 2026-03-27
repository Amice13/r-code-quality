# READ DATA

# remove missing values in left/right scale
d <- d[!is.na(d$leftright),]


# if (!bauer) d$left <- iconv(d$left, from="latin1", to="UTF-8")
# if (!bauer) d$right <- iconv(d$right, from="latin1", to="UTF-8")






#### LEFT ####



leftdfm <- dfm(d$left, ignoredFeatures=tm::stopwords("german"))
leftdfm <- trim(leftdfm, minDoc=3)

ridge <-  cv.glmnet(x=leftdfm, y=d$leftright,
	family="gaussian", alpha=0, nfolds=5, parallel=TRUE, 
	type.measure="mse")

# most predictive coefficients
best.lambda <- which(ridge$lambda==ridge$lambda.min)
beta <- ridge$glmnet.fit$beta[,best.lambda]

## identifying predictive features
df <- data.frame(coef = as.numeric(beta),
				word = names(beta), 
				freq = colSums(leftdfm),
				stringsAsFactors=F)

df <- df[order(df$coef),]
head(df[,c("coef", "word")], n=30)
paste(df$word[1:30], collapse=", ")
df <- df[order(df$coef, decreasing=TRUE),]
head(df[,c("coef", "word")], n=30)
paste(df$word[1:30], collapse=", ")

df$freq <- df$freq + runif(nrow(df), -1, 1)

## first attempt at figure... too crowded?
p <- ggplot(df, aes(x=coef, y=freq, size=freq, label=word, color=coef))
pq <- p + geom_text(alpha=.8) + scale_y_log10() + scale_size(trans="log") +
	theme_bw() + scale_color_continuous(low="red", high="blue")
pq

## trying a different approach
left.words <- data.frame(
	word = features(leftdfm),
	avg.lr = NA,
	freq = colSums(leftdfm),
	stringsAsFactors=F)

for (i in 1:nfeature(leftdfm)){
	left.words$avg.lr[i] <- mean(d$leftright[as.matrix(leftdfm[,i])>0])
}

# generating table
left.words <- left.words[order(left.words$avg.lr),]
tab <- matrix(NA, nrow=10, ncol=8)
tab[,1] <- head(left.words$word[left.words$freq>=5], n=10)
tab[,2] <- round(head(left.words$avg.lr[left.words$freq>=5], n=10),1)
tab[,3] <- tail(left.words$word[left.words$freq>=5], n=10)
tab[,4] <- round(tail(left.words$avg.lr[left.words$freq>=5], n=10),1)

# translate
tab[,1] <- car::recode(tab[,1], "'menschlichkeit'='humanity'; 'sicherung'='protection';
                             'bildung'='education'; 'kampf'='fight';
                             'demokratische'='democratic'; 'kinder'='children';
                             'bessere'='better'; 'gibt'='exists';
                             'frieden'='peace'; 'sicherheit'='security'")

tab[,3] <- car::recode(tab[,3], "'linksparteien'='leftparties'; 'schlechte'='bad';
                             'radikalismus'='radicalism'; 'linker'='more left';
                             'starke'='strong'; 'forderungen'='demands';
                             'eigentlich'='actually'; 'versprechungen'='promises';
                             'halt'='halt (mostly filling word)'; 'mag'='like'")



# adding a bit of jitter
left.words$freq <- left.words$freq + runif(nrow(left.words), -1, 1)

p <- ggplot(left.words, aes(x=avg.lr, y=freq, size=freq, label=word, color=avg.lr))
pq <- p + geom_text(alpha=.8) + scale_y_log10("Word frequency") + 
	scale_size(trans="log", guide="none") + theme_bw() + 
	scale_color_continuous(low="red", high="blue", guide="none") +
	scale_x_continuous("Average ideological position of individuals using each word")
pq

ggsave(pq, file="plots/left-words-by-ideology.pdf", height=6.5, width=14)







#### RIGHT ####


rightdfm <- dfm(d$right, ignoredFeatures=tm::stopwords("german"))
rightdfm <- trim(rightdfm, minDoc=3)

## trying a different approach
right.words <- data.frame(
	word = features(rightdfm),
	avg.lr = NA,
	freq = colSums(rightdfm),
	stringsAsFactors=F)

for (i in 1:nfeature(rightdfm)){
	right.words$avg.lr[i] <- mean(d$leftright[as.matrix(rightdfm[,i])>0])
}

# generating table
right.words <- right.words[order(right.words$avg.lr),]
tab[,5] <- head(right.words$word[right.words$freq>=5], n=10)
tab[,6] <- round(head(right.words$avg.lr[right.words$freq>=5], n=10),1)
tab[,7] <- tail(right.words$word[right.words$freq>=5], n=10)
tab[,8] <- round(tail(right.words$avg.lr[right.words$freq>=5], n=10),1)

# translate
tab[,5] <- car::recode(tab[,5], "'brutalität'='brutality'; 'ausgrenzung'='ostracism';
                             'konservatismus'='conservatism'; 'rechtradikale'='right-wing extremists';
                             'ungerechtigkeit'='injustice'; 'durchsetzen'='enforce';
                             'faschismus'='facism'; 'verboten'='illegal';
                             'krieg'='war'; 'reaktionär'='reactionist'")

tab[,7] <- car::recode(tab[,7], "'gesetze'='laws'; 'fall'='definitly';
                             'ausländern'='foreigners'; 'staat'='state';
                             'leben'='live'; 'nationalstolz'='national pride';
                             'nationale'='national'; 'gute'='good';
                             'vordergrund'='foreground'; 'gerechtigkeit'='justice'")

# generating table
Hmisc::latex(tab, file="")

# adding a bit of jitter
right.words$freq <- right.words$freq + runif(nrow(right.words), -1, 1)

p <- ggplot(right.words, aes(x=avg.lr, y=freq, size=freq, label=word, color=avg.lr))
pq <- p + geom_text(alpha=.8) + scale_y_log10("Word frequency") + 
	scale_size(trans="log", guide="none") + theme_bw() + 
	scale_color_continuous(low="red", high="blue", guide="none") +
	scale_x_continuous("Average ideological position of individuals using each word")
pq

ggsave(pq, file="plots/right-words-by-ideology.pdf", height=6.5, width=14)
