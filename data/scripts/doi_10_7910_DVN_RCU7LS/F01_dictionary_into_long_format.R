# Function to create a dictionary in long format

dictionary_into_long_format <- function(your_dictionary, partyname) {
  x <- melt(your_dictionary)
  x <- x[!is.na(x$value),]
  x <- x[!duplicated(x$value),]
  x$party <- partyname
  colnames(x) <- c("var", "category", "word", "party")
  return(x[,2:4])
}