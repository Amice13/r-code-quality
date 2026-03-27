api_secret <- 
api_key <- 
authURL <- 
accessURL <- 
requestURL <- 
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}

## This is only an example of Tweet collecting code. Replace the content of search to obtain all other tweets.
pbp <- searchTwitter("Police + Brutality + Protest", n=1000, lang ="en", since = '2015-10-15', until = '2015-10-22')
pbpdf <- twListToDF(pbp)
save(pbp, file = "pbp1022list")
save(pbpdf,file = "pbp1022df")
933 Tweet by Topsy
pbptxt <- sapply(pbp, function(x) x$getText())
pbpclean <- clean.text(pbptxt)
pbpclean <- toString(pbpclean)
pbpscore <- score.sentiment(pbpclean, pos.words, neg.words, .progress = 'text')
pbpscore$protest <- c("pbp")
write.csv(pbpdf, file = "/Users/NaijiaLiu/Dropbox/Fall2015/Thesis/pbpdf.csv", row.names = T)
pbpsubj <-score.subjectivity(pbpclean, strong.words, weak.words, .progress = 'text')
pbpsubj <- pbpsubj[c(-2)]
row.names(pbpsubj) <- "pbp"
