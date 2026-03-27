#set working directory
setwd("~/Desktop/Work/Fall 2017/Sampling/scripts_v5")

library(xml2)
library(stringr)

#define function to generate combinations of pairs for lists of more than 2 values
expand.grid.unique <- function(x, y, include.equals=FALSE)
{
  x <- unique(x)
  y <- unique(y)
  g <- function(i)
  {
    z <- setdiff(y, x[seq_len(i-include.equals)])
    if(length(z)) cbind(x[i], z, deparse.level=0)
  }
  do.call(rbind, lapply(seq_along(x), g))
}

data <- read_xml("discogs_20180901_masters.xml")

#chop up data from each release into something that can be ingested in a loop
entries <- as.character(xml_find_all(data, "//masters/master"))

#save all parsed entries
save(entries, file = "discogs_entries.RData")

#save only collaborative entries
collab_entries <- subset(entries, str_count(as.character(entries), "</id>") >= 2)
save(collab_entries, file = "discogs_collab_entries.RData")
rm(data, entries)

#go thru each collaborative release and extract the artist ids for each collaborator
edges <- data.frame()
i <- 1
while(i <= length(collab_entries)){
  temp <- unique(str_match_all(as.character(collab_entries[i]), "<id>(.*?)</id>")[[1]][,2])
  if(length(temp) >= 2){
    edges <- rbind(edges, as.data.frame(expand.grid.unique(temp, temp)))
  }
  
  #print counter
  cat(round(((i/length(collab_entries))*100), digits = 2), "%", "___", sep = "")
  
  #step up i
  i <- i + 1
}

write.csv(edges, file = "discogs_edges.csv", row.names = FALSE)