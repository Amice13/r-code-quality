##################################
# Load FAO GAEZ Data FUNCTIONS
# Carl MC, 16.8.2019
##################################


load_fao_keys <- function(data.path = "data"){
  read.csv(file.path(data.path,"geography", "FAO",  "allkeys.csv"), stringsAsFactors = F, sep = ";")
}


load_fao_cropsuit <- function(fao_key = NULL, crop = NULL, 
                              input_level = c("low", "high"), ## Select one
                              data_type = c("class", "index"), ### Select one
                              data.path = "data"){
  # Load keys
  fao.key.df <- load_fao_keys(data.path = data.path)
  
  # Subset
  fao.key.df <- fao.key.df[fao.key.df$type == "cropsuit", ]
  if(!is.null(fao_key)){
    fao.key.df <- fao.key.df[fao.key.df$fao_key %in% fao_key,]
  }
  if(!is.null(crop)){
    fao.key.df <- fao.key.df[fao.key.df$crop %in% crop,]
  }
  
  # Make path to files
  files <- file.path(data.path, "geography/FAO/cropsuit/",
                     paste0("res03_crav6190l",ifelse(data_type == "class", "si", "_sx"),
                            ifelse(input_level == "low", "lr","hr"), ifelse(data_type == "class", "", "_"),
                            fao.key.df$fao_key,".tif"))
  
  
  # Drop files that don't exist
  not.exist <- !file.exists(files)
  if(any(not.exist)){
    warning(paste("Could not find the following crops for ",  
                  input_level, "input level and ", data_type))
    warning(paste(fao.key.df[not.exist,"crop"], collapse = "; "))
  }
  files <- files[!not.exist]
  
  # Return files
  stack <- raster::stack(files)
  names(stack) <- fao.key.df$crop[!not.exist]
  stack
}


load_fao_misc <- function(type = NULL, fao_key = NULL, crop = NULL, data.path = "data"){
  if(any(type == "cropsuit")){
    warning("Does not load crop suitabilities. Use load_fao_cropsuit() instead")
  }
  
  # Load keys
  fao.key.df <- load_fao_keys(data.path = data.path)
  
  # Drop crop suitabilities
  fao.key.df <- fao.key.df[fao.key.df$type != "cropsuit",]
  
  # Subset
  if(!is.null(fao_key)){
    fao.key.df <- fao.key.df[fao.key.df$fao_key %in% fao_key,]
  }
  if(!is.null(crop)){
    fao.key.df <- fao.key.df[fao.key.df$crop %in% crop,]
  }
  if(!is.null(type)){
    fao.key.df <- fao.key.df[fao.key.df$type %in% type,]
  }
  
  # Make path to files
  files <- file.path(data.path, "geography/FAO/",fao.key.df$type,
                     paste0(fao.key.df$fao_key,".tif"))
  
  
  # Drop files that don't exist
  not.exist <- !file.exists(files)
  if(any(not.exist)){
    warning(paste("Could not find the following crops for ",  
                  input_level, "input level and ", data_type))
    warning(paste(fao.key.df[not.exist,"crop"], collapse = "; "))
  }
  files <- files[!not.exist]
  
  # Return files
  stack <- raster::stack(files)
  names(stack) <- fao.key.df$crop[!not.exist]
  stack
}


