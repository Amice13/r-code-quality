#set working directory
#setwd("~/file_path")

#whosampled datascraper --------------------------------------------------

#load packages
library(Rcrawler) #make sure this is v 0.1.2, using... devtools::install_version("Rcrawler", version = "0.1.2")
library(gsubfn)
library(xml2)
library(httr)

#store target URL
url <- as.character("http://www.whosampled.com/The-Winstons/Amen,-Brother/sampled/")

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
cat("Construct Links List")
while(linkcounter <= length(editlinks)){
  links <- unlist(LinkExtractor(editlinks[linkcounter], encod = "UTF-8", urlbotfiler = c("/buy/", "/share/", "/contact/", "/add/", "/search/", "/accounts/", "/forum/pm/", "/edit/", "/ajax", "/subscription/"))[2])
  linkslist <- c(linkslist, links[grep("whosampled.com/sample/", links)])
  
  cat(linkcounter, "...", sep = "")
  linkcounter = linkcounter + 1
}

#download HTML file for each sample event
i <- 1
while(i <= length(linkslist)){
  download.file(linkslist[i], destfile = paste("amen_brother/whosampled_data/", i, ".html", sep = ""), quiet = TRUE)
  cat(i, "...", sep = "")
  i <- i + 1
}

#get discogs artist ids for each artist, producer, and remixer involved in each song that sampled the target song
#any artist without a discogs page, or with a dead link, gets NA
discogs_ids <- list()
years <- c()
linkcounter <- 1
while(linkcounter <= length(linkslist)){
  #open HTML file and store as text
  html <- paste(readLines(paste("amen_brother/whosampled_data/", linkcounter, ".html", sep = "")), collapse = "\n")
  
  #get year from track page
  years[linkcounter] <- ContentScraper(html, patterns = c("//div[@class='trackLabel'][1]//span[@itemprop='datePublished']"), encod = "UTF-8", astext = TRUE)
  
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

save(years, file = "amen_brother/years.RData")
save(discogs_ids, file = "amen_brother/discogs_ids.RData")

remove(list = ls())

#albunack datascraper -------------------------------------------------------------------

#load packages
library(RSelenium)
library(Rcrawler)
library(xml2)
library(httr)

#instructions to setup and start server here: https://www.raynergobran.com/2017/01/rselenium-mac-update/
#first open Docker application
#then run the following in Terminal: docker run -d -p 4445:4444 selenium/standalone-firefox

browser <- remoteDriver(port = 4445L)

#simulate browser session
browser$open()

load("amen_brother/discogs_ids.RData")
trimmed_ids <- unique(unlist(discogs_ids))
trimmed_ids <- trimmed_ids[!is.na(trimmed_ids)]

musicbrainz_ids <- list()
i <- 1
while(i <= length(trimmed_ids)){
  browser$navigate(paste("http://www.albunack.net/artist/search?artistname=&musicbrainzartistid=&musicbrainzartistdbid=&discogsartistid=", trimmed_ids[i], sep = ""))
  albunack_url <- unlist(browser$getCurrentUrl())
  if(nchar(albunack_url) == 68){
    musicbrainz_ids[i] <- gsub(".html", "", gsub("http://artist.albunack.net/", "", albunack_url))
  }
  if(nchar(albunack_url) > 68){
    musicbrainz_ids[i] <- NA
  }
  if(nchar(albunack_url) < 68){
    musicbrainz_ids[i] <- "error"
  }
  cat(i, "...", sep = "")
  i <- i + 1
  Sys.sleep(1)
}

#retrieve any errors
i <- 1
while(i <= length(trimmed_ids)){
  if(musicbrainz_ids[i] == "error"){
    browser$navigate(paste("http://www.albunack.net/artist/search?artistname=&musicbrainzartistid=&musicbrainzartistdbid=&discogsartistid=", trimmed_ids[i], sep = ""))
    albunack_url <- unlist(browser$getCurrentUrl())
    if(nchar(albunack_url) == 68){
      musicbrainz_ids[i] <- gsub(".html", "", gsub("http://artist.albunack.net/", "", albunack_url))
    }
    if(nchar(albunack_url) > 68){
      musicbrainz_ids[i] <- NA
    }
    if(nchar(albunack_url) < 68){
      musicbrainz_ids[i] <- "error"
    }
    cat(i, "...", sep = "")
    i <- i + 1
    Sys.sleep(1)
  }
  else{
    i <- i + 1
    cat(i, "...", sep = "")
  }
}

save(musicbrainz_ids, file = "amen_brother/musicbrainz_ids.RData")

remove(list = ls())

#musicbrainz datascraper -------------------------------------------------

#https://github.com/mikkelkrogsholm/musicbrainz
library(musicbrainz)

#load data
load("amen_brother/musicbrainz_ids.RData")
load("amen_brother/discogs_ids.RData")
trimmed_ids <- unique(unlist(discogs_ids))
trimmed_ids <- trimmed_ids[!is.na(trimmed_ids)]

#remove all empty search results for both musicbrainz_ids and trimmed_ids
trimmed_ids <- trimmed_ids[!is.na(musicbrainz_ids)]
musicbrainz_ids <- musicbrainz_ids[!is.na(musicbrainz_ids)]

#retrieve metadata
name <- list()
gender <- list()
begin_area <- list()
area <- list()
i <- 1
while(i <= length(musicbrainz_ids)){
  temp <- mb_lookup("artist", musicbrainz_ids[i])
  name[i] <- temp$name
  gender[i] <- temp$gender
  begin_area[i] <- temp$begin_area$name
  area[i] <- temp$area$name
  cat(i, "...", sep = "")
  i <- i + 1
  Sys.sleep(0.6)
}

#null to NA, and replace any cut off end of list
name[sapply(name, is.null)] <- NA
if(length(name) < length(musicbrainz_ids)){
  name[(length(name) + 1):length(musicbrainz_ids)] <- NA
}

gender[sapply(gender, is.null)] <- NA
if(length(gender) < length(musicbrainz_ids)){
  gender[(length(gender) + 1):length(musicbrainz_ids)] <- NA
}

begin_area[sapply(begin_area, is.null)] <- NA
if(length(begin_area) < length(musicbrainz_ids)){
  begin_area[(length(begin_area) + 1):length(musicbrainz_ids)] <- NA
}

area[sapply(area, is.null)] <- NA
if(length(area) < length(musicbrainz_ids)){
  area[(length(area) + 1):length(musicbrainz_ids)] <- NA
}

#create data frame
artist_info <- data.frame(unlist(trimmed_ids), unlist(musicbrainz_ids), unlist(name), unlist(gender), unlist(begin_area), unlist(area))
colnames(artist_info) <- c("discogs", "musicbrainz", "name", "gender", "begin_area", "area")

#save data frame
save(artist_info, file = "amen_brother/artist_info.RData")

remove(list = ls())

#spotify datascraper -----------------------------------------------------

library(Rspotify)
library(stringr)

load("amen_brother/artist_info.RData")

keys <- spotifyOAuth("0123","37506eefcb334b409483e8413cf7900e","557de6395ec2451ba89ff8393635ea93")

names <- enc2utf8(as.character(unlist(artist_info$name)))
names <- unlist(artist_info$name)
names <- gsub("&amp;", "&", names)

popularity <- c()
followers <- c()

#go through stepwise, changing i and j to desired values until no errors are recorded...
i <- 1
j <- 100
while(i <= j){
  tryCatch({
    #retrieve search results from Spotify
    temp <- searchArtist(URLencode(names[i]), token = keys)
    if(nrow(temp) >= 1){
      popularity[[i]] <- temp$popularity[1]
      followers[[i]] <- temp$followers[1]
    }
    if(nrow(temp) == 0){
      popularity[[i]] <- NA
      followers[[i]] <- NA
    }
  },
  warning = function(w){
    print(paste("w: ", i))
    popularity[[i]] <- NA
    followers[[i]] <- NA
  },
  error = function(e){
    print(paste("e: ", i))
    popularity[[i]] <- NA
    followers[[i]] <- NA
  },
  finally = {
    i = i + 1
  })
}

spotify_data <- data.frame(names, popularity, followers)
save(spotify_data, file = "amen_brother/spotify_data.RData")

remove(list = ls())

#lat lon datascraper -----------------------------------------------------

library(ggmap)

#load necessary datasets
load("amen_brother/artist_info.RData")

#get begin area if it exists, and then area if begin area does not exist
area <- as.character(artist_info$begin_area)
area[which(area == "NA")] <- as.character(artist_info$area)[which(area == "NA")]
area[which(area == "NA")] <- NA
area_all <- names(table(area))

#get longitude and latitude of data science toolkit results for each location
lon <- c()
lat <- c()
area_df <- data.frame(lon, lat)
i <- 1
while(i <= length(area_all)){
  temp <- geocode(area_all[i], output = "latlon", source = "dsk")
  area_df[i, 1] <- temp[1, 1]
  area_df[i, 2] <- temp[1, 2]
  print(i)
  i <- i + 1
}

#fill in NAs generated by lack of results using google
j <- which(is.na(area_df[, 1]))
while(length(j) > 0){
  j <- which(is.na(area_df[, 1]))
  i <- 1
  while(i <= length(j)){
    temp <- geocode(area_all[j[i]], output = "latlon", source = "google")
    area_df[j[i], 1] <- temp[1, 1]
    area_df[j[i], 2] <- temp[1, 2]
    print(j[i])
    i <- i + 1
    Sys.sleep(1)
  }
}

lon <- c()
lat <- c()
i <- 1
while(i <= length(area)){
  if(!is.na(area[i])){
    lon <- c(lon, area_df[which(area_all == area[i]),1])
    lat <- c(lat, area_df[which(area_all == area[i]),2])
  }
  else{
    lon <- c(lon, NA)
    lat <- c(lat, NA)
  }
  i <- i + 1
}

id <- artist_info$discogs
geo_thesaurus <- data.frame(id, area, lon, lat)

save(geo_thesaurus, file = "amen_brother/geo_thesaurus.RData")

remove(list = ls())

#matrix construction -----------------------------------------------------

#read in edge list
edges <- read.csv(file = "discogs_edges.csv", header = TRUE, sep = ",")

#subset edge list by all pairs that have both nodes in the amen dataset
load("amen_brother/discogs_ids.RData")
trimmed_ids <- unique(unlist(discogs_ids))
trimmed_ids <- trimmed_ids[!is.na(trimmed_ids)]
edges <- edges[which(edges$V1 %in% trimmed_ids), ]
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

save(matrix, file = "amen_brother/matrix.RData")

remove(list = ls())

#events construction --------------------------------------------------------------

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

load("discogs_collab_entries.RData")
load("amen_brother/discogs_ids.RData")
trimmed_ids <- unique(unlist(discogs_ids))
trimmed_ids <- trimmed_ids[!is.na(trimmed_ids)]

#get index for all entries with at least one artist in dataset
i <- 1
index <- c()
while(i <= length(trimmed_ids)){
  temp <- grep(paste("<id>", trimmed_ids[i], "</id>", sep = ""), collab_entries)
  index <- c(index, temp)
  
  cat(i, "...", sep = "")
  i <- i + 1
}

#subset collab entries by index
index <- unique(index)
collab_entries <- collab_entries[index]

#go thru each collaborative release and extract the artist ids for each collaborator, as well as each year
edges <- data.frame()
years <- c()
i <- 1
while(i <= length(collab_entries)){
  temp1 <- unique(str_match_all(as.character(collab_entries[i]), "<id>(.*?)</id>")[[1]][,2])
  temp2 <- str_match_all(as.character(collab_entries[i]), "<year>(.*?)</year>")[[1]][,2]
  #only run loop if there are at least 2 unique artists, and a real year (empty ones are 0 for discogs)
  if((length(temp1) >= 2) & (nchar(temp2) == 4)){
    edges <- rbind(edges, as.data.frame(expand.grid.unique(temp1, temp1)))
    years <- c(years, rep(temp2, nrow(t(combn(temp1, 2)))))
  }
  
  #print counter
  cat(round(((i/length(collab_entries))*100), digits = 2), "%", "___", sep = "")
  
  #step up i
  i <- i + 1
}

events <- cbind(edges, years)
colnames(events) <- c("ind1", "ind2", "years")

events <- events[which(events$ind1 %in% trimmed_ids), ]
rownames(events) <- NULL
events <- events[which(events$ind2 %in% trimmed_ids), ]
rownames(events) <- NULL

save(events, file = "amen_brother/events.RData")

remove(list = ls())
