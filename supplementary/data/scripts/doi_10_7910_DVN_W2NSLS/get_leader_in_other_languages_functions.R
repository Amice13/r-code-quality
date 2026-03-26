getLeaderFromEN <- function(input_df, target_lang="fa") {
leaders <- input_df
leaders_en_name <- list()
leaders_name <- list()
    for (i in 1:nrow(input_df)) {
        print(i)
        
        ## if (is.na(leaders$name[i])) {
        leader <- leaders$en_name[i]
        
        temporaryFile <- tempfile()
        link <- paste0(
            "https://en.wikipedia.org/w/api.php?action=opensearch&search=",
            leader,
            "&limit=10&namespace=0&format=json"
        )
        download.file(link, destfile=temporaryFile, method="wget")
        
        json_data <- fromJSON(paste(readLines(temporaryFile), collapse=""))
        if (length(json_data[[4]])==0) {
            leaders_name[[i]] <- NA
            leaders_en_name[[i]] <- NA
            next
        }
        en_wiki <- strsplit(json_data[[4]][1], "/")[[1]][5]
        en_wiki_full <- json_data[[4]][1]
        
        link2 <- paste0("https://en.wikipedia.org/w/api.php?action=query&titles=",en_wiki,"&prop=langlinks&lllimit=500&format=json&redirects")
        temporaryFile2 <- tempfile()
        download.file(link2, destfile=temporaryFile2, method="wget")
        json_data2 <- fromJSON(paste(readLines(temporaryFile2), collapse=""))
        
        lang_links <- json_data2$query$pages[[1]]$langlinks
        which_in_list <- which(sapply(lang_links, function(x) x$lang==target_lang))
        if (length(which_in_list) != 0) {
            native_name <- lang_links[which_in_list][[1]]$`*`
        
        temporaryFile3 <- tempfile()
        link3 <- paste0(
            "https://",target_lang,
            ".wikipedia.org/w/api.php?action=opensearch&search=",
            native_name,
            "&limit=10&namespace=0&format=json"
        )
        download.file(link3, destfile=temporaryFile3, method="wget")
        
            json_data3 <- fromJSON(paste(readLines(temporaryFile3), collapse=""))

            native_wiki <- json_data3[[4]][1]
        } else {
            native_wiki <- paste("match in EN but no match in", toupper(target_lang))
        }


        
        leaders_name[[i]] <- native_wiki
        leaders_en_name[[i]] <- en_wiki_full
        
}
    return(data.frame(
               cia_name = leaders,
               en_wiki = unlist(leaders_en_name),
               native_wiki = unlist(leaders_name)
           ))
}

getWikisFromName <- function(input_df, source_lang="zh") {
    leaders <- input_df
leaders_en_name <- list()
leaders_name <- list()
    for (i in 1:nrow(leaders)) {
        print(i)
        
        leader <- leaders$name[i]

        temporaryFile <- tempfile()
        link <- paste0(
            "https://",
           source_lang,".wikipedia.org/w/api.php?action=opensearch&search=",
            leader,
            "&limit=10&namespace=0&format=json"
        )
        download.file(link, destfile=temporaryFile, method="wget")
        
        json_data <- fromJSON(paste(readLines(temporaryFile), collapse=""))
        leaders_name[[i]] <- json_data[[4]][1]
        if (is.null(json_data[[4]][1][[1]])) {
            leaders_name[[i]] <- NA
        }

        link2 <- paste0(
            "https://",
            source_lang,
            ".wikipedia.org/w/api.php?action=query&titles=",
            leader,
            "&prop=langlinks&lllimit=500&format=json&redirects"
        )
        temporaryFile2 <- tempfile()
        download.file(link2, destfile=temporaryFile2, method="wget")
        json_data2 <- fromJSON(paste(readLines(temporaryFile2), collapse=""))
        
        lang_links <- json_data2$query$pages[[1]]$langlinks
        if (!any(sapply(lang_links, function(x) x$lang=="en"))) {
            leaders_en_name[[i]] <- NA
            next
        }
        native_name <- lang_links[which(sapply(lang_links, function(x) x$lang=="en"))][[1]]$`*`
        
        temporaryFile3 <- tempfile()
        link3 <- paste0(
            "https://en.wikipedia.org/w/api.php?action=opensearch&search=",
            native_name,
            "&limit=10&namespace=0&format=json"
        )
        download.file(link3, destfile=temporaryFile3, method="wget")
        
        json_data3 <- fromJSON(paste(readLines(temporaryFile3), collapse=""))
        
        leaders_en_name[[i]] <- json_data3[[4]][1]
        
    }
    return(leader_df <- data.frame(
               cia_name = leaders,
               en_wiki = unlist(leaders_en_name),
               native_wiki = unlist(leaders_name)
           ))
}
