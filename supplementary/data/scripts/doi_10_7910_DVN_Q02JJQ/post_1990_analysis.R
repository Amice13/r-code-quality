#set working directory
#setwd("~/file_path")

#whosampled datascraper --------------------------------------------------

#load packages
library(Rcrawler)
library(gsubfn)
library(xml2)
library(httr)

links <- c()
i <- 1
while(i <= 10){
  temp_url <- paste("https://www.whosampled.com/most-sampled-tracks/", i, "/", sep = "")
  temp_links <- unlist(LinkExtractor(temp_url, encod = "UTF-8", urlbotfiler = c("/buy/", "/share/", "/contact/", "/add/", "/search/", "/accounts/", "/forum/pm/", "/edit/", "/ajax", "/subscription/"))[2])
  links <- c(links, temp_links[12:21])
  i <- i + 1
}

release_years <- c()
i <- 1
while(i <= 100){
  release_years <- c(release_years, unlist(ContentScraper(links[i], patterns = c("//span[@itemprop='datePublished']"), encod = "UTF-8", astext = TRUE)))
  i <- i + 1
}

scrape_links <- paste(links[which(release_years > 1990)], "sampled/", sep = "")

x <- 1
while(x <= length(scrape_links)){
  #store target URL
  url <- as.character(scrape_links[x])
  
  #get links for each page of results, from min to max
  links <- unlist(LinkExtractor(url, encod = "UTF-8", urlbotfiler = c("/buy/", "/share/", "/contact/", "/add/", "/search/", "/accounts/", "/forum/pm/", "/edit/", "/ajax", "/subscription/"))[2])
  targlinks <- links[grep("/?cp=", links)]
  nums <- as.integer(strapplyc(targlinks, "/?cp=(.*)", simplify = TRUE))
  
  #generate URLs for each page of results
  editlinks <- c()
  i = 1
  while(i <= max(nums)){
    editlinks <- c(editlinks, (paste(url, "?cp=", i, sep = "")))
    i = i+1
  }
  
  #get links to each individual song that sampled the target song
  linkslist = c()
  linkcounter = 1
  while(linkcounter <= length(editlinks)){
    links <- unlist(LinkExtractor(editlinks[linkcounter], encod = "UTF-8", urlbotfiler = c("/buy/", "/share/", "/contact/", "/add/", "/search/", "/accounts/", "/forum/pm/", "/edit/", "/ajax", "/subscription/"))[2])
    linkslist <- c(linkslist, links[grep("whosampled.com/sample/", links)])
    linkcounter = linkcounter + 1
  }
  
  #download HTML file for each sample event
  i <- 1
  while(i <= length(linkslist)){
    download.file(linkslist[i], destfile = paste("post_1990/whosampled_data_", x, "/", i, ".html", sep = ""), quiet = TRUE)
    cat(i, "...", sep = "")
    i <- i + 1
  }
  
  #get discogs artist ids for each artist, producer, and remixer involved in each song that sampled the target song
  #any artist without a discogs page, or with a dead link, gets NA
  linkcounter <- 1
  discogs_ids <- list()
  years <- c()
  while(linkcounter <= length(linkslist)){
    #open HTML file and store as text
    html <- paste(readLines(paste("post_1990/whosampled_data_", x, "/", linkcounter, ".html", sep = "")), collapse = "\n")
    
    #get year from track page
    years <- c(years, ContentScraper(html, patterns = c("//div[@class='trackLabel'][1]//span[@itemprop='datePublished']"), encod = "UTF-8", astext = TRUE))
    
    #loop to get all artists from particular track page
    i <- 1
    temp_list <- c()
    while(ContentScraper(html, patterns = c(paste("//div[@id='sampleWrap_dest']//div[@class='sampleTrackArtists']//a[", i, "]/@href", sep = "")), encod = "UTF-8", astext = TRUE) != "NA"){
      temp_list <- c(temp_list, ContentScraper(html, patterns = c(paste("//div[@id='sampleWrap_dest']//div[@class='sampleTrackArtists']//a[", i, "]/@href", sep = "")), encod = "UTF-8", astext = TRUE))
      i <- i + 1
    }
    
    #loop to get all remixers and producers from particular track page
    i <- 1
    while(ContentScraper(html, patterns = c(paste("//div[@class='track-metainfo-wrapper'][1]//div[@class='track-metainfo']//a[", i, "]/@href", sep = "")), encod = "UTF-8", astext = TRUE) != "NA"){
      temp_list <- c(temp_list, ContentScraper(html, patterns = c(paste("//div[@class='track-metainfo-wrapper'][1]//div[@class='track-metainfo']//a[", i, "]/@href", sep = "")), encod = "UTF-8", astext = TRUE))
      i <- i + 1
    }
    
    temp_list <- paste("https://www.whosampled.com", sub("\"", "", sub("href=\"", "", temp_list)), sep = "")
    
    #create list of discogs ids that are present
    i <- 1
    discogs_temp <- c()
    while(i <= length(temp_list)){
      j <- 1
      raw_list <- c()
      
      temp_html <- paste(GET(temp_list[i]), collapse = "\n")
      
      #enter NA into list if there are no links whatsoever, and if the artist page is a broken link in general
      tryCatch({
        if(ContentScraper(temp_html, patterns = c("//ul[@class='externalLinks']/li/a/@href"), encod = "UTF-8", astext = TRUE) == "NA"){
          discogs_temp <- c(discogs_temp, NA)
        }
        
        #retrieve and enter discogs id if there are links present
        else{
          while(ContentScraper(temp_html, patterns = c(paste("//ul[@class='externalLinks']/li[", j, "]/a/@href", sep = "")), encod = "UTF-8", astext = TRUE) != "NA"){
            raw_list <- c(raw_list, ContentScraper(temp_html, patterns = c(paste("//ul[@class='externalLinks']/li[", j, "]/a/@href", sep = "")), encod = "UTF-8", astext = TRUE))
            j <- j + 1
          }
          discogs_link <- sub("\"", "", sub("href=\"", "", raw_list[grep("discogs.com/", raw_list)]))
          discogs_temp <- c(discogs_temp, tryCatch({
            gsub("\\D", "", ContentScraper(discogs_link, patterns = c("//span[@class='copy_shortcut_code']"), encod = "UTF-8", astext = TRUE))
          }, error = function(e){
            return(NA)
          }))
        }
      }, error = function(e){
        discogs_temp <- c(discogs_temp, NA)
      })
      
      i <- i + 1
    }
    discogs_ids[[linkcounter]] <- unlist(discogs_temp)
    
    cat(linkcounter, "...", sep = "")
    linkcounter <- linkcounter + 1
  }
  
  save(years, file = paste("post_1990/years_", x, ".RData", sep = ""))
  save(discogs_ids, file = paste("post_1990/discogs_ids_", x, ".RData", sep = ""))
  x <- x + 1
}

remove(list = ls())

#matrix construction -----------------------------------------------------

#read in edge list
edges <- read.csv(file = "discogs_edges.csv", header = TRUE, sep = ",")
edges_raw <- edges

x <- 1
while(x <= 8){
  edges <- c()
  load(paste("post_1990/discogs_ids_", x, ".RData", sep = ""))
  
  #subset edge list by all pairs that have both nodes in the amen dataset
  trimmed_ids <- unique(unlist(discogs_ids))
  trimmed_ids <- trimmed_ids[!is.na(trimmed_ids)]
  edges <- edges_raw[which(edges_raw$V1 %in% trimmed_ids), ]
  rownames(edges) <- NULL
  edges <- edges[which(edges$V2 %in% trimmed_ids), ]
  rownames(edges) <- NULL
  
  #generate empty labeled matrix for data
  all_nodes <- sort(unique(c(as.numeric(unlist(edges[, 1])), as.numeric(unlist(edges[, 2])), as.numeric(unlist(trimmed_ids)))))
  matrix <- matrix(0, nrow = length(all_nodes), ncol = length(all_nodes))
  rownames(matrix) <- all_nodes
  colnames(matrix) <- all_nodes
  
  #go through the edge list, and add 1 to both cells in the matrix corresponding to each pair of nodes
  i <- 1
  while(i <= length(edges[, 1])){
    matrix[as.character(edges[i,])[1], as.character(edges[i,])[2]] <- matrix[as.character(edges[i,])[1], as.character(edges[i,])[2]] + 1
    matrix[as.character(edges[i,])[2], as.character(edges[i,])[1]] <- matrix[as.character(edges[i,])[2], as.character(edges[i,])[1]] + 1
    i <- i + 1
  }
  
  save(matrix, file = paste("post_1990/matrix_", x, ".RData", sep = ""))
  x <- x + 1
}

remove(list = ls())

#NBDA --------------------------------------------------------------------

source("NBDA code 1.2.13.R")

x <- 1
while(x <= 8){
  load(paste("post_1990/years_", x, ".RData", sep = ""))
  load(paste("post_1990/discogs_ids_", x, ".RData", sep = ""))
  load(paste("post_1990/matrix_", x, ".RData", sep = ""))
  years_raw <- years
  
  years <- c()
  weights <- c()
  i <- 1
  while(i <= length(rownames(matrix))){
    years_tally <- c()
    j <- 1
    while(j <= length(discogs_ids)){
      if(rownames(matrix)[i] %in% unlist(discogs_ids[j])){
        years_tally <- c(years_tally, years_raw[j])
      }
      j <- j + 1
    }
    weights <- c(weights, length(years_tally))
    years <- c(years, min(as.numeric(years_tally)))
    i <- i + 1
  }
  
  rownames(matrix) <- NULL
  colnames(matrix) <- NULL
  
  all_years <- names(table(sort(years)))
  
  orders <- c()
  i <- 1
  while(i <= length(years)){
    orders <- c(orders, which(all_years %in% years[i]))
    i <- i + 1
  }
  
  orders_nbda <- c()
  i <- 1
  while(i <= length(table(orders))){
    orders_nbda <- c(orders_nbda, which(orders == i))
    i <- i + 1
  }
  
  assign(paste("oa_", x, sep = ""), oaData(assMatrix = matrix, orderAcq = orders_nbda, groupid = as.character(x), weights = weights))
  save(list = paste("oa_", x, sep = ""), file = paste("post_1990/oa_", x, ".RData", sep = ""))
  cat(x, "...", sep = "")
  
  remove(list = c("discogs_ids", "matrix", "years_raw", "years_tally", "all_years", "i", "j", "orders", "orders_nbda", "years", "weights"))
  
  x <- x + 1
}

#different social learning parameter for each diffusion
sParamMatrix <- rbind(c(1, 2, 3, 4, 5, 6, 7))

#run all possible combinations of models! leaving out 5 because it's a producer tag
aic_table <- aicTableOA(oadata = c("oa_1", "oa_2", "oa_3", "oa_4", "oa_6", "oa_7", "oa_8"), group = FALSE, task = FALSE, sParamMatrix = sParamMatrix, aic = "aicc", pure = FALSE)

#run models
additive_oa <- addFit(oadata = c("oa_1", "oa_2", "oa_3", "oa_4", "oa_6", "oa_7", "oa_8"), bounded = FALSE, sParam = c(sParamMatrix))
