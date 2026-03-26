# IMPORT DATA FOR LEFT-RIGHT PROBING QUESTIONS
d <- read.dta("za4605_a08.dta", convert.underscore=TRUE, convert.factors=FALSE)
# d <- read.csv("za4605_a08_UTF8.csv", stringsAsFactors=F)
d$left <- d$f29
d$right <- d$f30
d$left <- sub("^\\s+", "", d$left)   # trim leading white space
d$left <- sub("^\\s+", "", d$left)   # again
d$left <- sub("^\\s+", "", d$left)   # again
d$right <- sub("^\\s+", "", d$right) # trim leading white space
d$right <- sub("^\\s+", "", d$right)   # again
d$right <- sub("^\\s+", "", d$right)   # again
# if you trim matching with wordstems does not work for single responses
d <- d[, c("v2", "v3", "left", "right")]
names(d) <- c("id", "east", "left", "right")


# IMPORT ALLBUS 2008 DATA (sociodemographics etc.)
ZA4600_A08 <- read.csv("ZA4600_A08.txt", sep="#")
names(ZA4600_A08)[2] <- "id"

# MERGE PROBING DATA WITH ALLBUS 2008
d <- join(d, ZA4600_A08, by="id", type="left")

# RECODE VARIABLES
# left-right scale - v106 - 1 = left; 10 = right
d$leftright <- d$v106
d$leftright[d$leftright==99] <- NA
table(d$leftright,d$v106)

# east: Interview in east germany or not
d$east[d$east==1] <- 0
d$east[d$east==2] <- 1

# education - v173
d$education <- NA
d$education[d$v173==1 | d$v173==2] <- 0
d$education[d$v173==3 | d$v173==4] <- 1
d$education[d$v173==5] <- 2
table(d$education,d$v173)

# age: 18-29 JAHRE; 30-44 JAHRE; 45-59 JAHRE; 60-74 JAHRE; 75-89 JAHRE; UEBER 89 JAHRE
d$age <- d$v155
d$age[d$age==9] <- NA
d$age <- d$age-1
table(d$age, d$v155)

# sex
d$male <- d$v151
d$male[d$male==2] <- 0 # 1 = male, 0 = female
table(d$male, d$v151)

# political interest: 2 = Very strong/strong, 1 = Middle, 0 = little/not at all
d$pol.interest <- NA
d$pol.interest[d$v100==4 | d$v100==5] <- 0
d$pol.interest[d$v100==3] <- 1
d$pol.interest[d$v100==1 | d$v100==2] <- 2
table(d$pol.interest, d$v100)

# income: higher values, higher income
d$income <- NA
d$income[d$v389>=0 & d$v389<=8] <- 0 # <= 999
d$income[d$v389>=9 & d$v389<=14] <- 1 # 1000 <= x <= 1999
d$income[d$v389>=15 & d$v389<=18] <- 2 # 2000 <= x <= 2999
d$income[d$v389>=19 & d$v389<=22] <- 3 # 3000 <= x
table(d$income, d$v389)

# party.preference
d$party.preference <- NA
d$party.preference[d$v70==1] <- "cdu"
d$party.preference[d$v70==2] <- "spd"
d$party.preference[d$v70==7] <- "linke"
d$party.preference[d$v70==3] <- "fdp"
d$party.preference[d$v70==4] <- "gruene"
d$party.preference[d$v70==5 | d$v70==6 | d$v70==8] <- "other.parties"
d$party.preference[d$v70==0] <- "none"
table(d$party.preference, d$v70)
d$factor.party.preference <- factor(d$party.preference, levels = c("none", "cdu", "spd", "gruene", "linke", "fdp", "other.parties"))





# Keep relevant variables
d <- d[,c("id", "east", "left", "right", "leftright", "education", "age", "male", "pol.interest", "income", "factor.party.preference")]


# START HERE


###########################
# IMPORT GESIS dictionary #
###########################



# IMPORT: Original dictionary was modified deleting some spaces and quotation marks
dictionary <- read.fwf("KATEGORIEN_100212_Master_deutsch_sort_modified.txt", width=c(4, 1, 1, 50), stringsAsFactors=F, encoding="")
dictionary <- data.frame(dictionary[-1,c(1,2,4)])
# V1 = topic number
# V2 = type of matching (blank = exact word, + = exact phrase, - = wordstem)
# V3 = dictionary expression
# Columns 1-4 = dictionarynumber; Columns 7-11 = "*****" followed by the real label

# EXTRACT TOPICS FROM dictionary
dic.topics <- data.frame(dictionary[substr(dictionary$V4, 1, 5)=="*****",]) # extract topics marked by *******
dic.topics$V4 <- substr(dic.topics$V4, 7, 30)  # delete stars
dic.topics <- dic.topics[,c(1,3)] # keep colums 1 and 3
dic.topics$V4 <- gsub(" ", ".", dic.topics$V4, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # replace spaces by .
dic.topics$V4 <- gsub("/", ".", dic.topics$V4, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # replace / by .
# write.table(dic.topics, "dic.topics.csv", sep=",", row.names=FALSE)
# DELETE dic.topics FROM dictionary FOR MATCHING LATER
dictionary <- data.frame(dictionary[substr(dictionary$V4, 1, 5)!="*****",])




# "THE GREAT LOOP": CODE LEFT-RIGHT CONTENT DATA WITH TOPICS FROM DICTIONARY
# LOOP TO MATCH GESIS DICTIONARY WITH CONTENT DATA - DO IT FOR d$left and d$right
# There must be priority in coding: First phrases(+), then exact words (blank), then wordstems (-)
# match exact word if V2 = blank; match exact phrase if V2 = +; match word stem if V2 = -
vars <- c("left", "right")
for (z in vars){
  ## 1) extract words that we will use to match against
  first <- tolower(dictionary$V4[dictionary$V2=="-"]) # WORDSTEMS (low priority)
  second <- tolower(dictionary$V4[dictionary$V2==" "]) # EXACT WORDS (middle priority)
  third <- tolower(dictionary$V4[dictionary$V2=="+"])  # WORD PHRASES (high priority)
  first[459:473] # Check
  first <- first[-c(459:473)] ## Remove
  ## 2) converting all responses to lower case
  responses <- tolower(d[,c(z)]) # d$left or d$right - REPLACE THROUGHOUT WHOLE LOOP
  ## 3) lowest priority: wordstems
  ## first, we add spaces around stems to make sure we're only matching beginnining of words
  ## (or the beginning of the response, denoted by ^)
  # stems <- paste0(" ", first, "|^", first, " | ", first, "$|^", first, "$")
  stems <- paste0(" ", first, "|^", first, " | ", first, "$|^", first, "$|^", first)
  stems.results <- lapply(stems, grep, responses)
  ## did this work?
  stems.results[[91]] ## these responses should mention 'exkommunis'
  responses[stems.results[[91]]]
  stems.results[[3]] ## these responses should mention 'kommunistisch'
  responses[stems.results[[3]]]

  ## 4) second lowest priority: exact words
  words <- paste0(" ", second, " |^", second, " | ", second, "$|^", second, "$")
  ## fixed so that responses with only ONE word are also matched
  words.results <- lapply(words, grep, responses)
  ## Check if it worked
  words[8]
  words.results[[8]] ## these responses should mention 'exkommunis'
  responses[words.results[[8]]]
  words[3]
  words.results[[3]] ## these responses should mention 'kommunistisch'
  responses[words.results[[3]]]

  ## 3) third set of matches: exact phrases
  phrases <- third
  phrases.results <- lapply(phrases, function(x) which(responses %in% x))

  ## Check if it worked
  phrases[1]
  phrases.results[[1]]
  responses[phrases.results[[1]]]
  categories <- rep(NA, length(d[,c(z)])) # empty array with categories
  ## Now a loop for each response...
  for (i in 1:length(d[,c(z)])){
    cat(i, " ")
    response <- d[,c(z)][i]
    # first of all, error trap: for empty responses, save as NA
    if (response==""){
      categories[i] <- NA
      next
    }
    ## if there was an exact match with a phrase...
    if(i %in% unlist(phrases.results)){
      # find what phrase it was (position on 'phrases' vector)
      match.position <- which(unlist(lapply(phrases.results, function(x) i %in% x))==TRUE)
      # and fill 'categories' array with that phrase
      categories[i] <- third[match.position]
      # and move to next response
      next
    }
    ## if not, check matches with exact words
    if(i %in% unlist(words.results)){
      # find what phrase it was (position on 'phrases' vector)
      match.position <- which(unlist(lapply(words.results, function(x) i %in% x))==TRUE)
      # if more than one match, choose match randomly
      if (length(match.position)>1){
        match.position <- sample(match.position, 1)
      }
      # and fill 'categories' array with that phrase
      categories[i] <- second[match.position]
      # and move to next response
      next
    }
    ## finally, if still nothing, check matches with stems
    if(i %in% unlist(stems.results)){
      # find what phrase it was (position on 'phrases' vector)
      match.position <- which(unlist(lapply(stems.results, function(x) i %in% x))==TRUE)
      # if more than one match, choose match randomly
      if (length(match.position)>1){
        match.position <- sample(match.position, 1)
      }
      # and fill 'categories' array with that phrase
      categories[i] <- first[match.position]
      # and move to next response
      next
    }
  }
  loopname <- paste("response.in.dictionary.", z, sep="") # name for new variable
  d[,c(loopname)] <- categories
}

## CHECKING RESULTS
test.r <- d[order(d$right),c("right", "response.in.dictionary.right")]
test.l <- d[order(d$left),c("left", "response.in.dictionary.left")]

# REPLACE EMPTY ANSWERS WITH "KEINE ANTWORT"
d$response.in.dictionary.left[d$left=="" & is.na(d$response.in.dictionary.left)] <- "keine antwort"
d$response.in.dictionary.right[d$right=="" & is.na(d$response.in.dictionary.right)] <- "keine antwort"

# MERGING WITH CATEGORY NUMBERS
dictionary$V4 <- tolower(dictionary$V4)
d <- merge(d, dictionary, all.x=TRUE, by.x="response.in.dictionary.left", by.y="V4")
names(d)[names(d)=="V1"] <- "category.nr.left"
names(d)[names(d)=="V2"] <- "match.type.left"
d <- merge(d, dictionary, all.x=TRUE, by.x="response.in.dictionary.right", by.y="V4")
names(d)[names(d)=="V1"] <- "category.nr.right"
names(d)[names(d)=="V2"] <- "match.type.right"

# REPLACE EMPTY BY MISSINGS
d$response.in.dictionary.left <- as.character(d$response.in.dictionary.left)
d$response.in.dictionary.right <- as.character(d$response.in.dictionary.right)
d$response.in.dictionary.left[is.na(d$response.in.dictionary.left)] <- "keine kategorie"
d$response.in.dictionary.right[is.na(d$response.in.dictionary.right)] <- "keine kategorie"

# ONLY KEEP RELEVANT VARIABLES AND REORDER
d <- d[,c("id", "leftright", "left", "response.in.dictionary.left", "category.nr.left", "match.type.left",
          "right", "response.in.dictionary.right", "category.nr.right", "match.type.right")]


# MERGE WITH NAMES OF SUBCATEGORIES
names(dic.topics) <- c("category.nr.right", "category.name") # name dataframe
dic.topics$category.nr.left <- dic.topics$category.nr.right # copy variable for merging (same name needed)
d <- join(d, dic.topics[,1:2], by="category.nr.right", type="left") # merge
names(d)[names(d)=="category.name"] <- "category.name.right" #
d <- join(d, dic.topics[,2:3], by="category.nr.left", type="left")
names(d)[names(d)=="category.name"] <- "category.name.left"


d <- d[order(d$id),]


# BAR PLOT TO SHOW THE NUMBER Of PEOPLE IN DIFFERENT CATEGORIES
pdf(file="plots/categorycount.pdf", width=9, height=3)
# windows(9,3)
par(mfrow=c(1,2), mar=c(4,8,1,1))
x <- sort(table(d$category.name.left), decreasing=TRUE)[1:10][rev(c(2,5,6,7,8,9,10,3,4,1))]
names.x <- c("No response", "Don't know", "Not assignable", "Politicians", "Socialism", "SPD", "Justice", "PDS, Left party", "Communism", "Solidarity")
cbind(names.x,gsub("(^)([[:alpha:]])", "\\1\\U\\2", str_replace_all(names(x), "[.]", " "), perl=TRUE))
barplot(x,las=2, horiz=TRUE, names.arg=names.x, xlim=c(0,800), cex.names=0.8, xlab="N in category", main="Associations with left", cex.main=.8)
x <- sort(table(d$category.name.right), decreasing=TRUE)[1:10][rev(c(2,3,4,6,8,9,10,5,7,1))]
names.x <- c("No response", "Don't know", "Not assignable", "Patriotism", "Xenophobia", "Conservativism", "Radical", "NPD, DVU, Republicans", "Right wing radicalism", "National socialism")
cbind(names.x,gsub("(^)([[:alpha:]])", "\\1\\U\\2", str_replace_all(names(x), "[.]", " "), perl=TRUE))
barplot(x,las=2, horiz=TRUE, names.arg=names.x, xlim=c(0,800), cex.names=0.8, xlab="N in category", main="Associations with right", cex.main=.8)
dev.off()



# PLOT MEANS FOR DIFFERENT SMALL CATEGORIES
# LEFT
vars <- names(sort(table(d$category.name.left), decreasing=TRUE)[1:14][-c(1,3,4)])
names <- c("Solidarity", "Communism", "Left party", "Justice", "SPD", "Socialism", "Politicians", "Radical", "Equality", "The people", "Real socialism")
means <- NULL; lowerci <- NULL; upperci <- NULL; means0 <- NULL; lowerci0 <- NULL; upperci0 <- NULL
for (i in vars){
  meani <- mean(d$leftright[d$category.name.left==i], na.rm=TRUE)
  means <- c(means,meani)
  n <- sum(!is.na(d$leftright[d$category.name.left==i]))
  sdev <- sd(d$leftright[d$category.name.left==i], na.rm=TRUE)
  lower <-     meani - qt(0.975,df=n-1)*sdev/sqrt(n-1)
  lowerci <- c(lowerci,lower)
  upper <-     meani + qt(0.975,df=n-1)*sdev/sqrt(n-1)
  upperci <- c(upperci,upper)
}
graphdata <- data.frame(rbind(upperci, means, lowerci))
names(graphdata) <- names
# GRAPH TO PLOT MEANS FOR TOPICS ON LEFT-RIGHT SCALE
pdf(file="plots/means_left_categories.pdf", width=6, height=2)
# windows(6,2)
plot.new()
par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot.window(xlim=c(-1,10), ylim=c(-1, 12))
lines(c(0,9), c(0,0))
for (i in 0:9){segments(i,-.4, i,.4)}
text(-1, 0, "left")
text(10, 0, "right")
y.positions <- seq(1,11,1)
points(graphdata[2,], y.positions, pch=19)
segments(mean(d$leftright, na.rm=TRUE),0, mean(d$leftright, na.rm=TRUE),11, lty=2)
for (i in 1:11){segments(graphdata[3,i],i,graphdata[1,i],i)}
for (i in 1:11){text(7,i, names(graphdata)[i], cex=.7)}
text(1,5, "Associations  \n with left", cex=.7)
for(i in 1:10){text(i-1,-1, as.character(i), cex=.6)}
dev.off()

# RIGHT
vars <- names(sort(table(d$category.name.right), decreasing=TRUE)[1:14][-c(1,5,6)])
names <- c("National Socialism", "Right wing radicalism", "Right wing parties", "Radical", "Conservatism", "Patriotism", "Xenophobia", "Violence","Capitalism", "Negative", "CDU")
cbind(vars,names)
means <- NULL; lowerci <- NULL; upperci <- NULL; means0 <- NULL; lowerci0 <- NULL; upperci0 <- NULL
for (i in vars){
  meani <- mean(d$leftright[d$category.name.right==i], na.rm=TRUE)
  means <- c(means,meani)
  n <- sum(!is.na(d$leftright[d$category.name.right==i]))
  sdev <- sd(d$leftright[d$category.name.right==i], na.rm=TRUE)
  lower <-     meani - qt(0.975,df=n-1)*sdev/sqrt(n-1)
  lowerci <- c(lowerci,lower)
  upper <-     meani + qt(0.975,df=n-1)*sdev/sqrt(n-1)
  upperci <- c(upperci,upper)
}
graphdata <- data.frame(rbind(upperci, means, lowerci))
names(graphdata) <- names
# GRAPH TO PLOT MEANS FOR TOPICS ON LEFT-RIGHT SCALE
pdf(file="plots/means_right_categories.pdf", width=6, height=2)
# windows(6,2)
plot.new()
par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot.window(xlim=c(-1,10), ylim=c(-1, 12))
lines(c(0,9), c(0,0))
for (i in 0:9){segments(i,-.4, i,.4)}
text(-1, 0, "left")
text(10, 0, "right")
y.positions <- seq(1,11,1)
points(graphdata[2,], y.positions, pch=19)
segments(mean(d$leftright, na.rm=TRUE),0, mean(d$leftright, na.rm=TRUE),11, lty=2)
for (i in 1:11){segments(graphdata[3,i],i,graphdata[1,i],i)}
for (i in 1:11){text(7,i, names(graphdata)[i], cex=.7)}
text(1,5, "Associations  \n with right", cex=.7)
for(i in 1:10){text(i-1,-1, as.character(i), cex=.6)}
dev.off()

