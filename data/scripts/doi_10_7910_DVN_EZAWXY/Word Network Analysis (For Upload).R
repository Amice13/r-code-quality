# Load required libraries
library(data.table)
library(plyr)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(ggplot2)
library(stm)
library(arrow)
library(stringr)
library(igraph)
library(visNetwork)
library(RColorBrewer)
library(splitstackshape)
library(purrr)
library(lubridate)
library(qgraph)
library(ForceAtlas2)
library(gephi)
library(Polychrome)
library(feather)

options(ggrepel.max.overlaps = Inf)


### Load and preprocess dataset
load("Abortion_Corpus_Preprocessed.RData")

# Reduce data size by selecting essential columns
result_key <- result_key[,!(names(result_key) %in% c("Period", "Content", "Content_WithIndex_trim"))]

# Rename "Fox News" to "Fox"
result_key[result_key$Press == "Fox News",]$Press <- "Fox"

# Define new period based on Trump's justice appointment
result_key$TwoPeriod_Press <- NA
result_key$TwoPeriod_Press[result_key$Press == "Fox" & as_date(result_key$Date) < as_date("2017-04-10")] <- "bef._Fox"
result_key$TwoPeriod_Press[result_key$Press == "Fox" & as_date(result_key$Date) >= as_date("2017-04-10")] <- "aft._Fox"
result_key$TwoPeriod_Press[result_key$Press == "MSNBC" & as_date(result_key$Date) < as_date("2017-04-10")] <- "bef._MSNBC"
result_key$TwoPeriod_Press[result_key$Press == "MSNBC" & as_date(result_key$Date) >= as_date("2017-04-10")] <- "aft._MSNBC"

### Keyword-specific corpus creation
Keyword <- "woman"  # Change keyword here as needed ("man", "baby", "abortion")
window <- 10  # Define window size for keyword extraction
keyword_regex <- paste0("([^\\s]+\\s+){0,", window, "}\\b(", Keyword, ")\\([aA-zZ_']+,[0-9]+\\)(\\s+[^\\s]+){0,", window, "}")

result_key$Content_Key <- sapply(result_key$Content_clean, function(x) {
  paste0(str_extract_all(x, keyword_regex)[[1]], collapse = "///")
})

# Filter rows without keywords
result_keyword <- result_key[nchar(result_key$Content_Key) != 0, ]

# Split rows by multiple occurrences
result_keyword_final <- cSplit(result_keyword, "Content_Key", sep = "///", direction = "long")
result_keyword_final$Content_Key <- as.character(result_keyword_final$Content_Key)

# Remove short entries
result_keyword_final <- result_keyword_final[str_count(result_keyword_final$Content_Key, '\\s') > 4, ]

# Save intermediate result
save(result_keyword_final, Keyword, window, file=paste0("raw_result_", Keyword, "_Window_", window, "_PaperVersion.RData"))

### Quanteda corpus and dfm creation
result_keyword_final$Content_Key_Untag <- gsub("([^\\(]+)(\\([^\\,]+,)([0-9]+\\))", "\\1", result_keyword_final$Content_Key)

corpus <- corpus(result_keyword_final, text_field="Content_Key_Untag")
tokens_news <- tokens(corpus, what="fastestword", remove_punct=FALSE)
tokens_final <- tokens_split(tokens_news, separator=" ", remove_separator=TRUE)
dfmat_key <- dfm(tokens_final)

# Save dfm results
save(result_keyword_final, dfmat_key, Keyword, window, file=paste0("raw_dfm_", Keyword, "_Window_", window, "_PaperVersion.RData"))

# Export vocabulary frequency for review
tstat_freq <- textstat_frequency(dfmat_key)
write.csv(tstat_freq, paste0("WordFre_", Keyword, "_Window_", window, ".csv"), row.names=FALSE)

### Word network analysis using igraph and Gephi
sub_list <- unique(dfmat_key@docvars$TwoPeriod_Press)

# Set TRUE if Gephi community info available
Gephi_use <- FALSE

for (period in sub_list) {
  dfmat_sub <- dfm_subset(dfmat_key, TwoPeriod_Press == period)
  dfmat_sub <- dfm_remove(dfmat_sub, c("woman", "abortion"))
  dfmat_sub_trim <- dfm_select(dfmat_sub, names(topfeatures(dfmat_sub, n=250)))
  dfmat_sub_trim <- dfm_subset(dfmat_sub_trim, ntoken(dfmat_sub_trim) > 0)
  
  # Co-occurrence matrix
  fcm_mat <- as.matrix(fcm(dfmat_sub_trim, tri=FALSE))
  diag(fcm_mat) <- 0
  cutoff <- quantile(fcm_mat, 0.99)
  fcm_mat[fcm_mat < cutoff] <- 0
  
  # Create igraph object
  graph <- graph.adjacency(fcm_mat, mode="undirected", weighted=TRUE, diag=FALSE)
  graph <- delete.vertices(simplify(graph), degree(graph)==0)
  
  # Community detection
  community <- cluster_louvain(graph)
  
  # Network layout (Fruchterman-Reingold)
  e <- get.edgelist(graph, names=FALSE)
  layout <- qgraph.layout.fruchtermanreingold(e, vcount=vcount(graph), area=100*vcount(graph)^2, niter=50000)
  
  # Plot network
  colors <- unname(dark.colors(max(membership(community))))
  tiff(filename=paste0("Word_Network_", period, "_", Keyword, ".tiff"), width=10, height=10, units='in', res=700)
  plot(community, graph,
       vertex.label.cex=1,
       edge.color="gray",
       edge.width=log(E(graph)$weight)*0.5,
       col=colors[membership(community)],
       vertex.frame.color=colors[membership(community)],
       vertex.label.dist=-0.7,
       vertex.size=sqrt(betweenness(graph))*0.1+1,
       layout=layout)
  dev.off()
  
  # Export edges for Gephi
  gephi_write_edges(graph, paste0("Edgelist_Gephi_", period, "_", Keyword, ".csv"))
}

