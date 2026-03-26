#############################
# LINGUISTIC MAPPING OF HISTORICAL ETHNIC DATA
# 
# This file adds the HEGs data to the language tree encapsuled in the LEDA project. 
#
# See for details: 
# https://github.com/carl-mc/LEDA
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#
# Called from scripts/data_prep/prepare_all.R.R
#
#############################

# Remove everything
rm(list = ls())


# FUNCTIONS ###################
expand_LEDA <- function(nastac_ethnic_match_dir = getwd()){
  # Packages
  require(LEDA)
  require(countrycode)
  
  
  # Make LEDA object
  leda <<- LEDA$new()
  
  
  # Add Ethnologue
  
  ## Ethnologue raw data
  ethno.new <- rbind(read.csv(file.path(nastac_ethnic_match_dir, 
                                        "matching_tables/Ethnologue16.csv")),
                     read.csv(file.path(nastac_ethnic_match_dir, 
                                        "matching_tables/Ethnologue16_added_groups.csv")))
  ethno.new$cowcode <- countrycode(ethno.new$iso3c,
                                   origin = "iso3c", 
                                   destination = "cown")
  leda$.__enclos_env__$private$ethno.df <- 
    rbind(leda$.__enclos_env__$private$ethno.df,
          ethno.new[,colnames(leda$.__enclos_env__$private$ethno.df)])
  leda$.__enclos_env__$private$ethno.df <- unique(leda$.__enclos_env__$private$ethno.df)
  
  ## Convert to long
  ethno.long.new <- ethno_wide2long(ethno.df = ethno.new,
                                    by.country = T, expand.dial = T)
  # Recode weird French case
  ethno.long.new$level9[ethno.long.new$level9 ==  'O\"il'] <- "Oil"
  
  ### Adjust ID
  ethno.long.new$ethno.id <- 
    ethno.long.new$ethno.id + max(leda$.__enclos_env__$private$ethno.long$ethno.id)
  
  ### Add
  leda$.__enclos_env__$private$ethno.long <- 
    rbind(leda$.__enclos_env__$private$ethno.long,
          ethno.long.new[,colnames(leda$.__enclos_env__$private$ethno.long)])
  
  ### Drop old "FOREIGN" links
  leda$.__enclos_env__$private$ethno.long <- 
    leda$.__enclos_env__$private$ethno.long[leda$.__enclos_env__$private$ethno.long$iso3c != "XXX",]
  leda$.__enclos_env__$private$ethno.long <-
    leda$.__enclos_env__$private$ethno.long[
      !duplicated(leda$.__enclos_env__$private$ethno.long[,!colnames(leda$.__enclos_env__$private$ethno.long) %in%
                                                            c("iso3c","ethno.id")]),
    ]
  
  
  # Add matches
  
  ## Matching tables to add
  
  ### Map matches
  files <- list.files(path = file.path(nastac_ethnic_match_dir, 
                                       "matching_tables"),
                      pattern = "\\.csv", full.names = F)
  files <- files[!grepl("thnologue|missing|nested", files)]
  
  ### Other
  files <- c(file.path("matching_tables", files),
             "amar_match/amr_fab_1.csv",
             "fearon_match/fearonlist.csv",
             "geoepr_match/geoepr_2019.csv",
             "greg_match/greg.csv")
  
  ## Add
  all.links <- do.call(rbind, lapply(files, function(f){
    # print(f)
    ## Load
    csv <- read.csv(file.path(nastac_ethnic_match_dir, f), stringsAsFactors = F)
    
    ## Rename vars
    csv$marker <- "language"
    csv$link <- csv$match
    csv$map <- gsub("\\.csv","",basename(f))
    csv$iso3c <- "EUR"
    
    ## Return
    csv[,c("group", "marker", "map", "link", "comment", "source", "iso3c")]
  }))
  
  
  ## Correct miss-spellings
  all.links$link <- trimws( all.links$link)
  all.links$link[all.links$link == "french [org] | Walloon [org]"] <- "French [org] | Walloon [org]"
  all.links$link[all.links$link == "aramaic [l4]"] <- "Aramaic"
  all.links$link[all.links$link == "Albanian [L2]"] <- "Albanian"
  all.links$link[all.links$link == "Jewish"] <- "Hebrew"
  all.links$link[all.links$link == "romani [l5]"] <- "Romani [l5]"
  all.links$link[grepl("Shor", all.links$link)] <- "Shor"
  all.links$link[grepl("Franco-Provencal", all.links$link)] <- "Franco-Provencal"
  all.links$link[all.links$link == "Crimean tatar [org] | Tatar [org]"] <- "Crimean Tatar | Tatar [org]"
  all.links$link[grepl("Kyrgyz", all.links$link)] <- "Kyrgyz"
  all.links$link <- gsub("Albanian [L2]",  "Albanian",
                         all.links$link, fixed = TRUE)
  all.links$link <- gsub("Yuruk [dial]",  'bgx$Yuruk',
                         all.links$link, fixed = TRUE)
  all.links$link <- gsub("Kistin [dial]",  'che$Kistin',
                         all.links$link, fixed = TRUE)
  all.links$link <- trimws( all.links$link)


  leda$add_tree_links(tree.link.df = all.links,
                      idvars = c("marker", "map", "iso3c"),
                   type = "NASTAC")  
  
  return(NULL)
}


# (function) Ethnologue wide --> long
#' @description Casts ethnologue data to wide, but retains all parent levels of each level
ethno_wide2long <- function(ethno.df, by.country = T, expand.dial = F){
  require(gtools)
  # Locals
  levels <- c(paste0("level", c(1:14)), "name", "iso", "dialect")
  
  # # Add non-african languages
  # if(add.nonafrican){
  #   nonafrican.df <- get_non_african_lang()
  #   ethno.df <- smartbind(ethno.df, nonafrican.df[!nonafrican.df$iso %in% ethno.df$iso,])
  # }
  
  
  # ... fill in all missing cells
  for(l in c(2:length(levels))){
    ethno.df[(is.na(ethno.df[,levels[l]]) | ethno.df[,levels[l]] == ""),levels[l]] <- 
      ethno.df[(is.na(ethno.df[,levels[l]]) | ethno.df[,levels[l]] == ""),levels[l-1]]
  }
  
  # Make long dataset
  
  # ... to long
  ethno.long <- ethno.df[rep(c(1:nrow(ethno.df)), length(levels)),c("iso3c",levels)]
  ethno.long$level <- rep(c(1:length(levels)), each = nrow(ethno.df))
  
  # ... generate entry for group name
  ethno.long$group <- unlist(ethno.df[,levels])
  
  # ... delete entries of children
  for(l in c(1:(length(levels)-1))){
    ethno.long[ethno.long$level == l, levels[c((l+1):length(levels))]] <- NA
  }
  
  # Make unique entries and drop those with missing / empty group names
  ethno.long <- unique(ethno.long)
  ethno.long <- ethno.long[!ethno.long$group %in% c(NA,""),]
  
  
  # Collapse over countries (if so wished)
  if(!by.country){
    ethno.long <- unique(ethno.long[,c(levels, "group")])
  } else {
    ethno.long <- do.call(smartbind, lapply(c(1:length(levels)), function(l){
      aggregate.data.frame(list(iso3c = ethno.long$iso3c[ethno.long$level == l]),
                           ethno.long[ethno.long$level == l,c("group","level",levels[1:l])],
                           FUN = function(x){paste(unique(x)[order(unique(x))], collapse = "|")})
    }))
    ethno.long <- ethno.long[,c("iso3c", "group", "level", levels)]
  }
  
  
  if(expand.dial){
    # ... make ethnologue ID
    ethno.long$ethno.id <- c(1:nrow(ethno.long))
    
    # ... add iso-code-expanded dialect
    exp.df <- ethno.long[ethno.long$level == max(ethno.long$level),]
    exp.df$dialect <- paste0(exp.df$iso, "$", exp.df$dialect)
    exp.df$group <- paste0(exp.df$iso, "$", exp.df$group)
    ethno.long <- rbind(ethno.long, exp.df)
  }
  
  # Return
  return(ethno.long)
}

# RUN ###################

# Expand LEDA Object

# Run function // Creates LEDA object in global environment
expand_LEDA(nastac_ethnic_match_dir = "../nastac_ethnic_match")

#  Save
saveRDS(leda, "data/geography/HEG/leda.rds")
saveRDS(leda, "replication/data/geography/HEG/leda.rds")
