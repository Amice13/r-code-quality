

# The steps leading up to the data frame used for STM estimation rely on the original pdf/html documents.
# The steps after line 219 rely on the dfm data frame uploaded with this script.

# Read all command papers after 1975, up to 2005; after that from govt website
all_cmd_7405 <- readtext("/Users/mp14106/Transcend/*/*.pdf",
                         docvarsfrom="filepaths",
                         dvsep="/",
                         verbosity=2
)
save(all_cmd_7405, file="all_cmd_7405.RData")


# Remove the first four uninformative docvars
all_cmd_7405$docvar1 <- NULL
all_cmd_7405$docvar2 <- NULL
all_cmd_7405$docvar3 <- NULL
all_cmd_7405$docvar4 <- NULL

# the useful docvars, coming from file paths
colnames(all_cmd_7405)[3] <- "file_year"
colnames(all_cmd_7405)[4] <- "file_name"

# These refer to the papers0523 data frame, for docs downloaded from the gov.uk portal.
papers0523$published <- as.Date(papers0523$published)
early <- which(papers0523$published<as.Date("2005-07-01"))
papers0523 <- papers0523[-early,]
yr05 <- which(all_cmd_7405$file_year==2005)

# remove from 7405 those included in the newer source
all_cmd_7405 <- all_cmd_7405[-yr05, ]

# start cleaning up the old docs
all_cmd_7405$text <- str_squish(all_cmd_7405$text) # remove whitespace

# remove proquest trademarking from pages
all_cmd_7405$text <- str_remove_all(all_cmd_7405$text, "House of Commons Parliamentary Papers Online.")
all_cmd_7405$text <- str_remove_all(all_cmd_7405$text, "Copyright")
all_cmd_7405$text <- str_remove_all(all_cmd_7405$text, "ProQuest Information and Learning Company.")
all_cmd_7405$text <- str_remove_all(all_cmd_7405$text, "All rights reserved.")
all_cmd_7405$text <- str_squish(all_cmd_7405$text) # remove whitespace again after cleainng trademarking

# remove whitespace from newer source as well
papers0523$text <- str_squish(papers0523$text)

# remove all doubles from both?
all_cmd_7405[setdiff(names(papers0523), names(all_cmd_7405))] <- NA
papers0523[setdiff(names(all_cmd_7405), names(papers0523))] <- NA

# merge the older and newer sources
cmd <- rbind(all_cmd_7405, papers0523)
long <- which(nchar(cmd$text)>1000000) # trim at more than approx 1M/6 words
cmd$text[long] <- substr(cmd$text[long], 1, 999999) # 78 were longer

# Note that file_year refers to different things before and after july 2005; before it is parl session
# Filter out international treaties:

i1 <- str_detect(substr(cmd$text, 1, 200), fixed("Treaty", ignore_case=TRUE))
i2 <- str_detect(substr(cmd$text, 1, 200), fixed("European Communities No.", ignore_case=TRUE))
i3 <- str_detect(substr(cmd$text, 1, 200), fixed("Miscellaneous", ignore_case=TRUE))
i4 <- str_detect(substr(cmd$text, 1, 200), fixed("Misellaneous", ignore_case=TRUE))
i5 <- str_detect(substr(cmd$text, 1, 200), fixed("between the Government of the United Kingdom", ignore_case=TRUE))
i6 <- str_detect(substr(cmd$text, 1, 200), fixed("between the United Kingdom", ignore_case=TRUE))
i7 <- str_detect(substr(cmd$text, 1, 200), fixed("European Union No.", ignore_case=TRUE))
i8 <- str_detect(substr(cmd$text, 1, 200), "CHAPTER")
i9 <- str_detect(substr(cmd$text, 1, 200), fixed("European Communities Series", ignore_case=TRUE))
i10<- str_detect(substr(cmd$text, 1, 200), fixed("European Union Series", ignore_case=TRUE))


itreaty <- i1|i2|i3|i4|i5|i6|i7|i8|i9|i10

# check for errors comparing their coding with the criterion
err <- which(cmd$typedoc=="international_treaty"&itreaty==F) # 14 some errors but debatable; out of 3495 sample; 988 treaty
err <- which(cmd$typedoc=="policy_paper"&itreaty==T) #24 but mostly wrong 13 an issue but reports on treaties

cmd$treaty <- itreaty

# check first few chars of some treaty 
rs <- sample(1:dim(cmd)[1], 10)
substr(cmd$text[rs][cmd$treaty[rs]], 1, 100)
substr(cmd$text[rs], 1, 100)

# define nontreaty
nontreaty=which(cmd$treaty==F)
# txt <- cmd$text[nontreaty]

# make all corpus
cmd <- corpus(cmd)

# subset to only non-treaties
cmdp <- corpus_subset(cmd, subset=!itreaty)

# the file docvar is just the basename of the file
docvars(cmdp)$file <- basename(names(cmdp))

# Spacy modelling, has to be chunked in four steps due to memory problems: 
spacy_initialize()

# break at 2469
cmd_sp <- spacy_parse(cmdp[2469], multithread=FALSE)
save(cmd_sp, file="cmd_sp_3.RData")
#spacy_finalize()

sp_res <- vector("list", length(cmdp)-3000)
for (i in 3001:length(cmdp)){
  #spacy_initialize()
  sp_res[[i]] <- spacy_parse(substr(cmdp[i], 1, 999999), multithread=FALSE)
  #cmd_sp <- rbind(cmd_sp, cmd_sp_t)
  #save(cmd_sp, file="cmd_sp_3.RData")
  #spacy_finalize()
  print(i)
}

save(sp_res, file="sp_res.RData")

rm(sp_res)

dfcmd_sp <- do.call(rbind, sp_res)

save(cmd_sp, file="cmd_sp_4.RData")

spacy_finalize()


# Read older datasets
load("cmd_sp_2full.RData")
cmd_sp1 <- cmd_sp
load("cmd_sp_3.RData")
cmd_sp2 <- cmd_sp

cmd_sp_all <- rbind(cmd_sp1, cmd_sp2, dfcmd_sp) # final Spacy results data frame
rm(list=setdiff(ls(), "cmd_sp_all"))
cmd_sp_all$doc_id <- as.factor(cmd_sp_all$doc_id)


# Extract non auxiliary verbs from above: 
cmd_sp_vb <- cmd_sp_all[cmd_sp_all$pos=="VERB",]

cmd_vb_ag <- aggregate(cmd_sp_vb$lemma, by=list(cmd_sp_vb$doc_id), paste, collapse = " ")
cmd_vb_ag <- corpus(cmd_vb_ag, docid_field="Group.1", text_field="x")
cmd_vb_ag <- tokens(cmd_vb_ag, remove_symbols=T)

cmd_dfmat <- dfm(cmd_vb_ag) # The dfmat of verbs

# Move other objects to disk
save(cmd_sp_vb, file="cmd_sp_vb.RData")
rm(cmd_sp_vb)
save(cmd_sp_all, file="cmd_sp_all.RData")
rm(cmd_sp_all)
save(cmd_vb_ag, file="cmd_vb_ag.RData")
rm(cmd_vb_ag)

load("cmd_sp_all.RData") # to extract the nouns

cmd_sp_noun <- cmd_sp_all[cmd_sp_all$pos=="NOUN",]

rm(cmd_sp_all)

cmd_noun_ag <- aggregate(cmd_sp_noun$lemma, by=list(cmd_sp_noun$doc_id), paste, collapse = " ")
cmd_noun_ag <- corpus(cmd_noun_ag, docid_field="Group.1", text_field="x")
cmd_noun_ag <- tokens(cmd_noun_ag, remove_symbols=T)

cmd_noun <- dfm(cmd_noun_ag)
topfeatures(cmd_noun)
rm(cmd_noun_ag)

# Merge docvars to the above, from separate cmdp data frame. 

docvars(cmd_dfmat) <- dvtemp
docvars(cmd_dfmat)$file_year[which(docvars(cmd_dfmat)$updated>as.Date("2023-01-01"))] <- 2023


# Examine the dfmat of verbs before any processing
topfeatures(cmd_dfmat)

# Keep only English words
hunspell::dictionary(lang = "en_GB")
english_words <- readLines("/Users/mp14106/Library/R/arm64/4.3/library/hunspell/dict/en_GB.dic") %>% gsub("/.+", "", .)
sample(english_words, 20)
english_words <- tolower(english_words)

cmd_dfmat_clean <- dfm_keep(cmd_dfmat, pattern=english_words, min_nchar=2) # Keep English, remove single chars

cmd_dfmat_clean <- dfm_subset(cmd_dfmat_clean, ntoken(cmd_dfmat_clean) > 0) # Remove any empty docs

cmd_noun_clean <- dfm_keep(cmd_noun, pattern=english_words, min_nchar=2) # Same for nouns data frame.

docvars(cmd_dfmat_clean)$file_year <- as.numeric(docvars(cmd_dfmat_clean)$file_year)
docvars(cmd_dfmat_clean)$publishedfull <- docvars(cmd_dfmat_clean)$published

# Older docs are classified by parliamentary session. Put in eg 1 March 1985 for all Oct 1984-Oct 85 docs.
for (i in 1974:2004){
  docvars(cmd_dfmat_clean)$publishedfull[docvars(cmd_dfmat_clean)$file_year==i] <- as.Date(paste(i+1,"-03-01", sep=""))
}
docvars(cmd_dfmat_clean)$publishedfull <- as.numeric(docvars(cmd_dfmat_clean)$publishedfull)

# Classify dates by govt.
docvars(cmd_dfmat_clean)$gov <- NA
docvars(cmd_dfmat_clean)$gov[docvars(cmd_dfmat_clean)$publishedfull<as.Date("1979-05-04")] <- "Old Labour"
docvars(cmd_dfmat_clean)$gov[docvars(cmd_dfmat_clean)$publishedfull>as.Date("1979-05-04") & docvars(cmd_dfmat_clean)$publishedfull<as.Date("1997-05-01")] <- "Thatcher-Major"
docvars(cmd_dfmat_clean)$gov[docvars(cmd_dfmat_clean)$publishedfull>as.Date("1997-05-01") & docvars(cmd_dfmat_clean)$publishedfull<as.Date("2010-05-11")] <- "New Labour"
docvars(cmd_dfmat_clean)$gov[docvars(cmd_dfmat_clean)$publishedfull>as.Date("2010-05-11") & docvars(cmd_dfmat_clean)$publishedfull<as.Date("2016-07-15")] <- "Conservative Cameron"
docvars(cmd_dfmat_clean)$gov[docvars(cmd_dfmat_clean)$publishedfull>as.Date("2016-07-15")] <- "Conservative Brexit"


# Save data for replication: 
save(cmd_dfmat_clean, file="cmd_dfmat_clean.RData")


# K=2 model does not show much separation: 
s2 <- stm(cmd_dfmat_clean, K=2, prevalence=~s(file_year, 10), data=docvars(cmd_dfmat_clean), seed=123)
summary(s2)
e2 <- estimateEffect(s2, formula=~s(file_year, 10), metadata=docvars(cmd_dfmat_clean))
plot.estimateEffect(e2, model=s2, covariate="file_year", method="continuous", printlegend=T, ci.level=.95, labeltype="score" )


# This is the main K=3 model from table 2
s3 <- stm(cmd_dfmat_clean, K=3, prevalence=~s(publishedfull, 10), data=docvars(cmd_dfmat_clean), seed=123)
summary(s3)
e3 <- estimateEffect(s3, formula=~s(publishedfull, 10), metadata=docvars(cmd_dfmat_clean))

# Figure 1
plot.estimateEffect(e3, model=s3, covariate="publishedfull", method="continuous", printlegend=T, ci.level=.95, labeltype="score", main="K=3", ylim=c(0, .85), xaxt="n")
lines(x=c(as.numeric(as.Date("1979-05-04")),as.Date("1979-05-04")), y=c(-.1,.65) , type="l")
text(x=as.numeric(as.Date("1979-05-04")+2200), y=0, labels="Thatcher-Major")
lines(x=c(as.numeric(as.Date("1997-05-01")),as.Date("1997-05-01")), y=c(-.1,.65) , type="l")
text(x=as.numeric(as.Date("1997-05-01")+1900), y=0, labels="New Labour")
lines(x=c(as.numeric(as.Date("2010-05-11")),as.Date("2010-05-11")), y=c(-.1,.65) , type="l")
text(x=as.numeric(as.Date("2010-05-11")+900), y=0, labels="Cam")
lines(x=c(as.numeric(as.Date("2016-07-15")),as.Date("2016-07-15")), y=c(-.1,.65) , type="l")
text(x=as.numeric(as.Date("2016-07-15-01")+1100), y=0, labels="Brexit")

# Figure 6
par(mar=c(5,9,4,1)+.1)
plot.estimateEffect(e3, model=s3, covariate="publishedfull", method="difference", cov.value1=as.Date('2022-07-01'), cov.value2=as.Date("2015-07-01"), printlegend=T, ci.level=.95, verbose.labels=F , labeltype="score", xlab="Proportion change 2022 vs 2015", main="K=3") 
dev.off()

par(mar=c(5,9,4,1)+.1)
plot.estimateEffect(e3, model=s3, covariate="publishedfull", method="difference", cov.value1=as.Date('2015-07-01'), cov.value2=as.Date("2010-07-01"), printlegend=T, ci.level=.95, verbose.labels=F , labeltype="score", xlab="Proportion change 2015 vs 2010", main="K=3") 
dev.off()

# representative docs
summary(s3)
labelTopics(s3, n=15)
findThoughts(s3, texts=docvars(cmd_dfmat_clean)$file, topics=2, n=10)
findThoughts(s3, texts=docvars(cmd_dfmat_clean)$file, topics=3, n=10)

# K=4 to 50 models below: 
s4 <- stm(cmd_dfmat_clean, K=4, prevalence=~s(publishedfull, 10), data=docvars(cmd_dfmat_clean), seed=123)
summary(s4)
e4 <- estimateEffect(s4, formula=~s(publishedfull, 10), metadata=docvars(cmd_dfmat_clean))

# Figure 2
plot.estimateEffect(e4, model=s4, covariate="publishedfull", method="continuous", printlegend=T, ci.level=.95, labeltype="score", main="K=4", ylim=c(0, .8), xaxt="n")
lines(x=c(as.numeric(as.Date("1979-05-04")),as.Date("1979-05-04")), y=c(-.1,.58) , type="l")
text(x=as.numeric(as.Date("1979-05-04")+2200), y=0, labels="Thatcher-Major")
lines(x=c(as.numeric(as.Date("1997-05-01")),as.Date("1997-05-01")), y=c(-.1,.58) , type="l")
text(x=as.numeric(as.Date("1997-05-01")+1900), y=0, labels="New Labour")
lines(x=c(as.numeric(as.Date("2010-05-11")),as.Date("2010-05-11")), y=c(-.1,.58) , type="l")
text(x=as.numeric(as.Date("2010-05-11")+900), y=0, labels="Cam")
lines(x=c(as.numeric(as.Date("2016-07-15")),as.Date("2016-07-15")), y=c(-.1,.58) , type="l")
text(x=as.numeric(as.Date("2016-07-15-01")+1100), y=0, labels="Brexit")


s5 <- stm(cmd_dfmat_clean, K=5, prevalence=~s(publishedfull, 10), data=docvars(cmd_dfmat_clean), seed=123)
summary(s5)
e5 <- estimateEffect(s5, formula=~s(publishedfull, 10), metadata=docvars(cmd_dfmat_clean))

findThoughts(s5, texts=docvars(cmd_dfmat_clean)$file, topics=1, n=10)

# Figure 3
dev.off()
plot.estimateEffect(e5, model=s5, covariate="publishedfull", method="continuous", printlegend=T, ci.level=.95, labeltype="score", main="K=5", ylim=c(0, .65), xaxt="n")
lines(x=c(as.numeric(as.Date("1979-05-04")),as.Date("1979-05-04")), y=c(-.1,.44) , type="l")
text(x=as.numeric(as.Date("1979-05-04")+2200), y=0, labels="Thatcher-Major")
lines(x=c(as.numeric(as.Date("1997-05-01")),as.Date("1997-05-01")), y=c(-.1,.44) , type="l")
text(x=as.numeric(as.Date("1997-05-01")+1900), y=0, labels="New Labour")
lines(x=c(as.numeric(as.Date("2010-05-11")),as.Date("2010-05-11")), y=c(-.1,.44) , type="l")
text(x=as.numeric(as.Date("2010-05-11")+900), y=0, labels="Cam")
lines(x=c(as.numeric(as.Date("2016-07-15")),as.Date("2016-07-15")), y=c(-.1,.44) , type="l")
text(x=as.numeric(as.Date("2016-07-15-01")+1100), y=0, labels="Brexit")



s10 <- stm(cmd_dfmat_clean, K=10, prevalence=~s(publishedfull, 10), data=docvars(cmd_dfmat_clean), seed=123)
summary(s10)
e10 <- estimateEffect(s10, formula=~s(publishedfull, 10), metadata=docvars(cmd_dfmat_clean))

# Figure 4
dev.off()
plot.estimateEffect(e10, model=s10, covariate="publishedfull", method="continuous", printlegend=T, ci.level=0, labeltype="score", main="K=10", ylim=c(0, .65), xaxt="n")
lines(x=c(as.numeric(as.Date("1979-05-04")),as.Date("1979-05-04")), y=c(-.1,.30) , type="l")
text(x=as.numeric(as.Date("1979-05-04")+2200), y=0, labels="Thatcher-Major")
lines(x=c(as.numeric(as.Date("1997-05-01")),as.Date("1997-05-01")), y=c(-.1,.30) , type="l")
text(x=as.numeric(as.Date("1997-05-01")+1900), y=0, labels="New Labour")
lines(x=c(as.numeric(as.Date("2010-05-11")),as.Date("2010-05-11")), y=c(-.1,.30) , type="l")
text(x=as.numeric(as.Date("2010-05-11")+900), y=0, labels="Cam")
lines(x=c(as.numeric(as.Date("2016-07-15")),as.Date("2016-07-15")), y=c(-.1,.30) , type="l")
text(x=as.numeric(as.Date("2016-07-15-01")+1100), y=0, labels="Brexit")

par(mar=c(5,9,4,1)+.1)
plot.estimateEffect(e10, model=s10, covariate="publishedfull", method="difference", cov.value1=as.Date('2022-07-01'), cov.value2=as.Date("2015-07-01"), printlegend=T, ci.level=.95, verbose.labels=F , labeltype="score", xlab="Proportion change 2022 vs 2015", main="K=10") 

plot.estimateEffect(e10, model=s10, covariate="publishedfull", method="difference", cov.value1=as.Date('2015-07-01'), cov.value2=as.Date("2010-07-01"), printlegend=T, ci.level=.95, verbose.labels=F , labeltype="score", xlab="Proportion change 2022 vs 2015", main="K=10") 
dev.off()

plot.estimateEffect(e10, model=s10, covariate="publishedfull", method="difference", cov.value1=as.Date('1985-07-01'), cov.value2=as.Date("1977-07-01"), printlegend=T, ci.level=.95, verbose.labels=F , labeltype="score", xlab="Proportion change 2022 vs 2015", main="K=10") 


findThoughts(s10, texts=docvars(cmd_dfmat_clean)$file, topics=4, n=10)


s50 <- stm(cmd_dfmat_clean, K=50, prevalence=~s(publishedfull, 10), data=docvars(cmd_dfmat_clean), seed=123)
summary(s50)
e50 <- estimateEffect(s50, formula=~s(publishedfull, 10), metadata=docvars(cmd_dfmat_clean))


# Figure 5
dev.off()
plot.estimateEffect(e50, model=s50, topics=c(2, 4, 6, 11, 27, 29, 41), covariate="publishedfull", method="continuous", printlegend=T, ci.level=0, labeltype="score", main="K=50", ylim=c(0, .29), xaxt="n")
lines(x=c(as.numeric(as.Date("1979-05-04")),as.Date("1979-05-04")), y=c(-.1,.13) , type="l")
text(x=as.numeric(as.Date("1979-05-04")+2200), y=0, labels="Thatcher-Major")
lines(x=c(as.numeric(as.Date("1997-05-01")),as.Date("1997-05-01")), y=c(-.1,.13) , type="l")
text(x=as.numeric(as.Date("1997-05-01")+1900), y=0, labels="New Labour")
lines(x=c(as.numeric(as.Date("2010-05-11")),as.Date("2010-05-11")), y=c(-.1,.13) , type="l")
text(x=as.numeric(as.Date("2010-05-11")+900), y=0, labels="Cam")
lines(x=c(as.numeric(as.Date("2016-07-15")),as.Date("2016-07-15")), y=c(-.1,.13) , type="l")
text(x=as.numeric(as.Date("2016-07-15-01")+1100), y=0, labels="Brexit")


labelTopics(s50, topics=35, n=15)
findThoughts(s50, texts=docvars(cmd_dfmat_clean)$file, topics=35, n=10)

## noun topic model

s20top <- stm(cmd_noun_clean, K=20, prevalence=~s(publishedfull, 10), data=docvars(cmd_dfmat_clean), seed=123)

e20top <- estimateEffect(s20top, formula=~s(publishedfull, 10), metadata=docvars(cmd_dfmat_clean))

# Evolution of noun topics over time: 
for (i in 1:20){
  plot.estimateEffect(e20top, topic=i, model=s20top,covariate="publishedfull", method="continuous", ci=0, labeltype="score", main=paste("Topic", i))
}
summary(s1top)
plot(s1top)

# Extract noun topic proportions for control later: 
topprop <- data.frame(s1top$theta)
topprop <- cbind(docvars(cmd_noun_clean)$file, topprop)

# Merge into other dataset: 

topprop20 <- data.frame(s20top$theta)
colnames(topprop20) <- c("n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8", "n9", "n10", "n11", "n12", "n13", "n14", "n15", "n16", "n17", "n18", "n19", "n20")
topprop20 <- cbind(docvars(cmd_noun_clean)$file, topprop20)
colnames(topprop20)[1] <- "file"
docvars(cmd_dfmat_clean) <- merge(docvars(cmd_dfmat_clean), topprop20, by="file", sort=F) 

head(docvars(cmd_dfmat_clean))

## models with the controls
# K=3

s3c20 <- stm(cmd_dfmat_clean, K=3, prevalence=~s(publishedfull, 10)+n2+n3+n4+n5+n6+n7+n8+n9+n10+n11+n12+n13+n14+n15+n16+n17+n18+n19+n20, data=docvars(cmd_dfmat_clean), seed=123)
summary(s3c20)

e3c20 <- estimateEffect(s3c20, formula=~s(publishedfull, 10)+n2+n3+n4+n5+n6+n7+n8+n9+n10+n11+n12+n13+n14+n15+n16+n17+n18+n19+n20, metadata=docvars(cmd_dfmat_clean))

# Split the topics into economic and non-economic - not much difference: 
docvars(cmd_dfmat_clean)$ecprop <- docvars(cmd_dfmat_clean)$X2+docvars(cmd_dfmat_clean)$X3+docvars(cmd_dfmat_clean)$X6+docvars(cmd_dfmat_clean)$X7+docvars(cmd_dfmat_clean)$X9
docvars(cmd_dfmat_clean)$ecprop <- docvars(cmd_dfmat_clean)$X2+docvars(cmd_dfmat_clean)$X7


# Full graph with K=3: Y axis does not mean much
plot.estimateEffect(e3c20, model=s3c20, covariate="publishedfull", method="continuous", printlegend=T, ci.level=.95, labeltype="score", main="K=3, with topic controls", ylim=c(0, .8), xaxt="n")
lines(x=c(as.numeric(as.Date("1979-05-04")),as.Date("1979-05-04")), y=c(-.1,.62) , type="l")
text(x=as.numeric(as.Date("1979-05-04")+2200), y=0, labels="Thatcher-Major")
lines(x=c(as.numeric(as.Date("1997-05-01")),as.Date("1997-05-01")), y=c(-.1,.62) , type="l")
text(x=as.numeric(as.Date("1997-05-01")+1900), y=0, labels="New Labour")
lines(x=c(as.numeric(as.Date("2010-05-11")),as.Date("2010-05-11")), y=c(-.1,.62) , type="l")
text(x=as.numeric(as.Date("2010-05-11")+900), y=0, labels="Cam")
lines(x=c(as.numeric(as.Date("2016-07-15")),as.Date("2016-07-15")), y=c(-.1,.62) , type="l")
text(x=as.numeric(as.Date("2016-07-15-01")+1100), y=0, labels="Brexit")


# Just topic 2, for Figure 8 in paper: 

plot.estimateEffect(e3c20, model=s3c20, topics=2, covariate="publishedfull", method="continuous", printlegend=T, ci.level=.95, labeltype="score", main="K=3, with topic controls", xaxt="n", ylim=c(.27, .55))
lines(x=c(as.numeric(as.Date("1979-05-04")),as.Date("1979-05-04")), y=c(-.1,.50) , type="l")
text(x=as.numeric(as.Date("1979-05-04")+2200), y=.28, labels="Thatcher-Major")
lines(x=c(as.numeric(as.Date("1997-05-01")),as.Date("1997-05-01")), y=c(-.1,.50) , type="l")
text(x=as.numeric(as.Date("1997-05-01")+1900), y=.28, labels="New Labour")
lines(x=c(as.numeric(as.Date("2010-05-11")),as.Date("2010-05-11")), y=c(-.1,.50) , type="l")
text(x=as.numeric(as.Date("2010-05-11")+900), y=.28, labels="Cam")
lines(x=c(as.numeric(as.Date("2016-07-15")),as.Date("2016-07-15")), y=c(-.1,.50) , type="l")
text(x=as.numeric(as.Date("2016-07-15-01")+1100), y=.28, labels="Brexit")





# controls, s10


# controls s10 c 20

s10c20 <- stm(cmd_dfmat_clean, K=10, prevalence=~s(publishedfull, 10)+n2+n3+n4+n5+n6+n7+n8+n9+n10+n11+n12+n13+n14+n15+n16+n17+n18+n19+n20, data=docvars(cmd_dfmat_clean), seed=123)
summary(s10c20)
e10c20 <- estimateEffect(s10c20, formula=~s(publishedfull, 10)+n2+n3+n4+n5+n6+n7+n8+n9+n10+n11+n12+n13+n14+n15+n16+n17+n18+n19+n20, metadata=docvars(cmd_dfmat_clean))
#e3 <- estimateEffect(s3, formula=~gov, metadata=docvars(cmd_dfmat_clean))

# For figure 8 in paper: 
dev.off()
plot.estimateEffect(e10c20, model=s10c20, topics=c(2), covariate="publishedfull", method="continuous", printlegend=T, ci.level=.95, labeltype="score", main="K=10, with topic controls", xaxt="n", ylim=c(-.20, .22))
lines(x=c(as.numeric(as.Date("1979-05-04")),as.Date("1979-05-04")), y=c(-.25,.16) , type="l")
text(x=as.numeric(as.Date("1979-05-04")+2200), y=-.2, labels="Thatcher-Major")
lines(x=c(as.numeric(as.Date("1997-05-01")),as.Date("1997-05-01")), y=c(-.25,.16) , type="l")
text(x=as.numeric(as.Date("1997-05-01")+1900), y=-.2, labels="New Labour")
lines(x=c(as.numeric(as.Date("2010-05-11")),as.Date("2010-05-11")), y=c(-.25,.16) , type="l")
text(x=as.numeric(as.Date("2010-05-11")+900), y=-.2, labels="Cam")
lines(x=c(as.numeric(as.Date("2016-07-15")),as.Date("2016-07-15")), y=c(-.25,.16) , type="l")
text(x=as.numeric(as.Date("2016-07-15-01")+1100), y=-.2, labels="Brexit")





# K=50 and topic controls
s50c20 <- stm(cmd_dfmat_clean, K=50, prevalence=~s(publishedfull, 10)+n2+n3+n4+n5+n6+n7+n8+n9+n10+n11+n12+n13+n14+n15+n16+n17+n18+n19+n20, data=docvars(cmd_dfmat_clean), seed=123)
summary(s50c20)
e50c20 <- estimateEffect(s50c20, formula=~s(publishedfull, 10)+n2+n3+n4+n5+n6+n7+n8+n9+n10+n11+n12+n13+n14+n15+n16+n17+n18+n19+n20, metadata=docvars(cmd_dfmat_clean))

plot.estimateEffect(e50c20, model=s50c20, covariate="publishedfull", method="difference", cov.value1=as.Date('2022-07-01'), cov.value2=as.Date("1985-07-01"), printlegend=T, ci.level=.95, verbose.labels=F , xlab="Proportion change 2022 vs 1980", main="K=50") 
# increases: 2, 4, 29, 41, 6, 


plot.estimateEffect(e50c20, model=s50c20, topics=c(2, 4,  11, 6, 29, 27, 41), covariate="publishedfull", method="continuous", printlegend=T, ci.level=0, labeltype="score", main="K=50, with topic controls", ylim=c(-.05, .30), xaxt="n")
lines(x=c(as.numeric(as.Date("1979-05-04")),as.Date("1979-05-04")), y=c(-.07,.11) , type="l")
text(x=as.numeric(as.Date("1979-05-04")+2200), y=-.05, labels="Thatcher-Major")
lines(x=c(as.numeric(as.Date("1997-05-01")),as.Date("1997-05-01")), y=c(-.07,.11) , type="l")
text(x=as.numeric(as.Date("1997-05-01")+1900), y=-.05, labels="New Labour")
lines(x=c(as.numeric(as.Date("2010-05-11")),as.Date("2010-05-11")), y=c(-.07,.11) , type="l")
text(x=as.numeric(as.Date("2010-05-11")+900), y=-.05, labels="Cam")
lines(x=c(as.numeric(as.Date("2016-07-15")),as.Date("2016-07-15")), y=c(-.07,.11) , type="l")
text(x=as.numeric(as.Date("2016-07-15-01")+1100), y=-.05, labels="Brexit")

par(mar=c(5,9,4,1)+.1)
plot.estimateEffect(e50c20, model=s50c20, topics=c(2, 4,  11, 6, 29, 27, 41), covariate="publishedfull", method="difference", cov.value1=as.Date('2022-07-01'), cov.value2=as.Date("1985-07-01"), printlegend=T, ci.level=.95, verbose.labels=F , xlab="Proportion change 2022 vs 1985", main="K=50", labeltype="score") 




docvars(cmd_dfmat_clean)$publishedmo <- floor_date(as.Date(docvars(cmd_dfmat_clean)$publishedfull), "month")

# To examine docs with more than .70 activist category, text in section 4:
ts3 <- data.frame(s3$theta)
ts3 <- cbind(ts3, docvars(cmd_dfmat_clean))
write.csv(ts3, "~/research/bk/ts3.csv")

t2docs <- which(ts3$X2>.70)
t2docse <- which(ts3$X2>.70 & ts3$file_year<1997)
t2docsm <- which(ts3$X2>.70 & ts3$file_year>=1997 & ts3$file_year<2002)


t2docsl <- which(ts3$X2>.70 & ts3$file_year>2016)

ts3$file[t2docse]
ts3$file[t2docsm]
ts3$file[t2docsl]





### Dependencies work for table 2: 
load("cmd_sp_all.RData")
# Activist verbs across all K: 
awords <- unique(c(labelTopics(s3, n=10)$score[2,], labelTopics(s4, n=10)$score[4,], labelTopics(s5, n=10)$score[5,],labelTopics(s10, n=10)$score[2,]))

cmd_sp_all$id <- paste(cmd_sp_all$doc_id, cmd_sp_all$sentence_id, sep=".")
ac_ind <- c(which(cmd_sp_all$lemma == awords[1]), which(cmd_sp_all$lemma == awords[2]), which(cmd_sp_all$lemma == awords[3]), which(cmd_sp_all$lemma == awords[4]),which(cmd_sp_all$lemma == awords[5]), which(cmd_sp_all$lemma == awords[6]), which(cmd_sp_all$lemma == awords[7]), which(cmd_sp_all$lemma == awords[8]),which(cmd_sp_all$lemma == awords[9]), which(cmd_sp_all$lemma == awords[10]), which(cmd_sp_all$lemma == awords[11]),which(cmd_sp_all$lemma == awords[12]))
ac_sent_ind <- cmd_sp_all$id[ac_ind]
cmd_act <- cmd_sp_all[cmd_sp_all$id %in% ac_sent_ind,]


# Aggregate by docid sentenceid - recover the text of sentences with activist words

cmd_act_ag <- aggregate(cmd_act$token, by=list(cmd_act$doc_id), paste, collapse = " ")
cmd_act_corpus <- corpus(cmd_act_ag, docid_field="Group.1", text_field="x")

# Spacy dependency parsing on those sentences: 
spacy_initialize()

cmd_dep <- spacy_parse(cmd_act_corpus, multithread=FALSE, pos=TRUE, tag=TRUE, dependency=TRUE)
cmd_dep <- nounphrase_consolidate(cmd_dep)

save(cmd_dep, file="cmd_dep.RData")


cmd_dep$headfull <- paste(cmd_dep$doc_id, cmd_dep$sentence_id, cmd_dep$head_token_id, sep=".")
cmd_dep$idfull <- paste(cmd_dep$doc_id, cmd_dep$sentence_id, cmd_dep$token_id, sep=".")

# For each of the active words, extract their direct and prepositional objects: 
# Results in table 2:
 
onelink_lemmas_all <- as.character(NULL)
onelink_docs_all <- as.character(NULL)
for (i in 1:15){
  act1 <- which((cmd_dep$lemma==awords[i]) &(cmd_dep$pos=="VERB")) 
  onelink <- which(cmd_dep$headfull %in% cmd_dep$idfull[act1])
  onelink_lemmas <- cmd_dep$lemma[onelink][(cmd_dep$dep_rel[onelink] %in% c("dobj", "pobj"))]
  onelink_doc <- basename(cmd_dep$doc_id[onelink][(cmd_dep$dep_rel[onelink] %in% c("dobj", "pobj"))])
  onelink_lemmas_all <- c(onelink_lemmas_all, onelink_lemmas)
  onelink_docs_all <- c(onelink_docs_all, onelink_doc)
}

onelink_df <- data.frame(onelink_docs_all, onelink_lemmas_all) 
onelink_df <- merge(onelink_df, docvars(cmd_dfmat), by.x="onelink_docs_all", by.y="file", all.x=T, all.y=F, sort=F)


tail(sort(table(onelink_df$onelink_lemmas_all)), 20) # Look at the top 20 



onelink_df$publishedfull <- onelink_df$published

# Build a dfm of objects, to model over time: 
for (i in 1974:2004){
  onelink_df$publishedfull[onelink_df$file_year==i] <- as.Date(paste(i+1,"-03-01", sep=""))
}
onelink_df$publishedfull <- as.numeric(onelink_df$publishedfull)



objects_ag <-  aggregate(onelink_df$onelink_lemmas_all, by=list(onelink_df$onelink_docs_all), paste, collapse = " ")
objects_ag_p <-  aggregate(onelink_df$publishedfull, by=list(onelink_df$onelink_docs_all), FUN=head, 1)
objects_ag_c <- cbind(objects_ag, objects_ag_p)
colnames(objects_ag_c) <- c("doc_id", "text", "file", "publishedfull")
objects_ag_y_c <- corpus(objects_ag_c, docid_field="doc_id", text_field="text")
objects_ag_y_t <- tokens(objects_ag_y_c, )

objects_dfm <- dfm(objects_ag_y_t)
objects_dfm <- dfm_subset(objects_dfm, ntoken(objects_dfm)>0)
objects_dfm <- dfm_subset(objects_dfm, !(is.na(objects_dfm$publishedfull)))
objects_dfm <- dfm_keep(objects_dfm, pattern=english_words, min_nchar=2)
objects_dfm <- dfm_subset(objects_dfm, ntoken(objects_dfm)>0)




o1 <- stm(objects_dfm, K=2, prevalence=~s(publishedfull, 10), seed=123)
summary(o1)

o3 <- stm(objects_dfm, K=3, prevalence=~s(publishedfull, 10), seed=123)


# Figure 11 in paper: 

eo3 <- estimateEffect(o3, formula=~s(publishedfull, 10), metadata=docvars(objects_dfm))
plot.estimateEffect(eo3, model=o3, covariate="publishedfull", method="continuous", labeltype="prob", ylim=c(.1, 1), n=6 ,xaxt="n")
lines(x=c(as.numeric(as.Date("1979-05-04")),as.Date("1979-05-04")), y=c(-.1,.82) , type="l")
text(x=as.numeric(as.Date("1979-05-04")+2200), y=.1, labels="Thatcher-Major")
lines(x=c(as.numeric(as.Date("1997-05-01")),as.Date("1997-05-01")), y=c(-.1,.82) , type="l")
text(x=as.numeric(as.Date("1997-05-01")+1900), y=.1, labels="New Labour")
lines(x=c(as.numeric(as.Date("2010-05-11")),as.Date("2010-05-11")), y=c(-.1,.82) , type="l")
text(x=as.numeric(as.Date("2010-05-11")+900), y=.1, labels="Cam")
lines(x=c(as.numeric(as.Date("2016-07-15")),as.Date("2016-07-15")), y=c(-.1,.82) , type="l")
text(x=as.numeric(as.Date("2016-07-15-01")+1100), y=.1, labels="Brexit")





# modelling subjects

act1 <- which((cmd_dep$lemma==labelTopics(s3, n=7)$score[3,7]) &(cmd_dep$pos=="VERB")) 
onelink <- which(cmd_dep$headfull %in% cmd_dep$idfull[act1])
onelink_lemmas <- cmd_dep$lemma[onelink][(cmd_dep$dep_rel[onelink] %in% c("nsubj"))]
onelink_lemmas_nsubj <- as.character(NULL)

for (i in 1:15){
  act1 <- which((cmd_dep$lemma==awords[i]) &(cmd_dep$pos=="VERB")) 
  onelink <- which(cmd_dep$headfull %in% cmd_dep$idfull[act1])
  onelink_lemmas <- cmd_dep$lemma[onelink][(cmd_dep$dep_rel[onelink] %in% c("nsubj"))]
  onelink_lemmas_nsubj <- c(onelink_lemmas_nsubj, onelink_lemmas)
}

tail(sort(table(onelink_lemmas_nsubj)), 20) # Results in table 2

# budgets
budgets <- readtext("/Users/mp14106/research/bk/budgets/*.pdf",
                    docvarsfrom="filenames",
                    dvsep="/",
                    verbosity=2
)

for (i in 1:50){
  print(i)
  print(nchar(budgets$text[i]))
}

budgets <- corpus(budgets)
budgets$text <- str_squish(budgets$text)
budgets$text <- str_remove_all(budgets$text, "House of Commons Parliamentary Papers Online.")
budgets$text <- str_remove_all(budgets$text, "Copyright")
budgets$text <- str_remove_all(budgets$text, "ProQuest Information and Learning Company.")
budgets$text <- str_remove_all(budgets$text, "All rights reserved.")
budgets$text <- str_squish(budgets$text)

longb <- which(nchar(budgets)>1000000)


budgets[longb] <- substr(budgets[longb], 1, 999999) 
spacy_initialize()
cmd_budgets <- spacy_parse(budgets, multithread=FALSE)
cmd_budgets_vb <- cmd_budgets[cmd_budgets$pos=="VERB",]
cmd_budgets_ag <- aggregate(cmd_budgets_vb$lemma, by=list(cmd_budgets_vb$doc_id), paste, collapse = " ")
cmd_budgets_ag <- corpus(cmd_budgets_ag, docid_field="Group.1", text_field="x")
cmd_budgets_ag <- tokens(cmd_budgets_ag, remove_symbols=T)
cmd_budgets_dfmat <- dfm(cmd_budgets_ag)
docvars(cmd_budgets_dfmat)$year <- rownames(cmd_budgets_dfmat)
docvars(cmd_budgets_dfmat)$year <- str_sub(docvars(cmd_budgets_dfmat)$year, 1, -5)
docvars(cmd_budgets_dfmat)$year <- as.numeric(docvars(cmd_budgets_dfmat)$year)
cmd_budgets_dfmat <- dfm_keep(cmd_budgets_dfmat, pattern=english_words, min_nchar=2)

save(cmd_budgets, file="cmd_budgets.RData")
rm(cmd_budgets)
s1b <- stm(cmd_budgets_dfmat, K=3, prevalence=~s(year, 10), data=docvars(cmd_budgets_dfmat), seed=123)
summary(s1b)
e1b <- estimateEffect(s1b, formula=~s(year, 10), metadata=docvars(cmd_budgets_dfmat))
plot.estimateEffect(e1b, method="continuous", covariate="year")

s2b <- stm(cmd_budgets_dfmat, K=2, prevalence=~s(year, 10), data=docvars(cmd_budgets_dfmat), seed=123)
summary(s2b)

# For figure 9 in paper: 
e2b <- estimateEffect(s2b, formula=~s(year, 10), metadata=docvars(cmd_budgets_dfmat))
plot.estimateEffect(e2b, model=s2b, method="continuous", covariate="year", labeltype="score", ylim=c(-.02, 1.5), linecol=c("red", "blue"), n=6)
lines(x=c(as.numeric(as.Date("1979-05-04")),as.Date("1979-05-04")), y=c(-.1,.82) , type="l")
text(x=as.numeric(as.Date("1979-05-04")+2200), y=.1, labels="Thatcher-Major")
lines(x=c(as.numeric(as.Date("1997-05-01")),as.Date("1997-05-01")), y=c(-.1,.82) , type="l")
text(x=as.numeric(as.Date("1997-05-01")+1900), y=.1, labels="New Labour")
lines(x=c(as.numeric(as.Date("2010-05-11")),as.Date("2010-05-11")), y=c(-.1,.82) , type="l")
text(x=as.numeric(as.Date("2010-05-11")+900), y=.1, labels="Cam")
lines(x=c(as.numeric(as.Date("2016-07-15")),as.Date("2016-07-15")), y=c(-.1,.82) , type="l")
text(x=as.numeric(as.Date("2016-07-15-01")+1100), y=.1, labels="Brexit")


### White papers analysis

load("dvdfmwp.R")
head(dvdfmwp)
setdiff(dvdfmwp$file, docvars(cmd_dfmat_clean)$file)
head(docvars(cmd_dfmat_clean))
docvars(cmd_dfmat_clean)$wp <- docvars(cmd_dfmat_clean)$file %in% dvdfmwp$file

cmd_dfmat_clean_wp <- cmd_dfmat_clean[cmd_dfmat_clean$wp==T, ]
cmd_dfmat_clean_wp


s3wp <- stm(cmd_dfmat_clean_wp, K=3, prevalence=~s(publishedfull, 10), data=docvars(cmd_dfmat_clean_wp), seed=123)
summary(s3wp)
labelTopics(s3wp,n=20)
findThoughts(s3wp, texts=docvars(cmd_dfmat_clean_wp)$file, topics=2, n=30)
e3wp <- estimateEffect(s3wp, formula=~s(publishedfull, 10), metadata=docvars(cmd_dfmat_clean_wp))
plot.estimateEffect(e3wp, model=s3wp, covariate="publishedfull", topics=c(1,2,3), method="continuous", printlegend=T, ci.level=0, verbose.labels=F , labeltype="score", main="K=3") 



# Figure 10 in paper: 
plot.estimateEffect(e3wp, model=s3wp, covariate="publishedfull", method="continuous", printlegend=T, ci.level=.95, labeltype="score", main="K=3", ylim=c(0, 1), xaxt="n")
lines(x=c(as.numeric(as.Date("1979-05-04")),as.Date("1979-05-04")), y=c(-.1,.65) , type="l")
text(x=as.numeric(as.Date("1979-05-04")+2200), y=0, labels="Thatcher-Major")
lines(x=c(as.numeric(as.Date("1997-05-01")),as.Date("1997-05-01")), y=c(-.1,.65) , type="l")
text(x=as.numeric(as.Date("1997-05-01")+1900), y=0, labels="New Labour")
lines(x=c(as.numeric(as.Date("2010-05-11")),as.Date("2010-05-11")), y=c(-.1,.65) , type="l")
text(x=as.numeric(as.Date("2010-05-11")+900), y=0, labels="Cam")
lines(x=c(as.numeric(as.Date("2016-07-15")),as.Date("2016-07-15")), y=c(-.1,.65) , type="l")
text(x=as.numeric(as.Date("2016-07-15-01")+1100), y=0, labels="Brexit")



### spacy on sample sentence

spacy_initialize()

test_sentence <- c("The government will develop a scheme to help innovative businesses.")
test_sentence <- corpus(test_sentence)

sp_test <- spacy_parse(test_sentence, multithread=FALSE, pos=TRUE, tag=TRUE, dependency=TRUE)





