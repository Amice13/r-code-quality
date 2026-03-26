
### Setting working directory
setwd("/YOUR/WORKING DIRECTORY/HERE")

### installing packages
install.packages("vkR")
install.packages("dplyr")
install.packages("ggmap")
install.packages("sp")
install.packages("geosphere")
install.packages("rgdal")
install.packages("sf")
Yinstall.packages("mapview")
install.packages("varhandle")
### loading packages
library(vkR) ### IMPORTANT: You will need to set the language to Ukrainian in your Vkontakte account settings for the code below to work. 
library(dplyr)
library(ggmap)
library(sp)
library(geosphere)
library(rgdal)
library(sf)
library(mapview)
library(varhandle)

### Using VkR to interract with VK's API. You need to register a new standalone app in VK to fill in the information below:https://vk.com/dev/standalone
## Documentation and a guide is available on vkR's page: https://github.com/Dementiy/vkR
vkOAuth(client_id = INSERT_YOUR_CLINET_ID_HERE, 'groups, walls, friends')
setAccessToken(access_token = 'INSERT_YOUR_ACCESS_TOKEN_HERE')


###########################################################################
##################Finding public users in Crimea###########################
###########################################################################
###downloading a list o regions in Ukraine

regions <- databaseGetRegions(country_id = 2)
### converting into a dataframe
regions <- as.data.frame(regions$items) 

# download a list with all cities in Crimea. 
citiescrimea<- databaseGetCities(country_id = 2, region_id = 1500001, count = 1000)
#converting to dataframe
citiescrimea <- as.data.frame(citiescrimea$items)

# download a list with all cities in Kherson Oblast 
citieskherson<- databaseGetCities(country_id = 2, region_id = 1512710, count = 1000)
#converting to dataframe
citieskherson <- as.data.frame(citieskherson$items)

# download a list with all cities in Kherson Oblast 
citieszaporizhia<- databaseGetCities(country_id = 2, region_id = 1504503, count = 1000)
#converting to dataframe
citieszaporizhia <- as.data.frame(citieszaporizhia$items)

### removing the empty "important variable"
citieszaporizhia$important <- NULL


### Merging Crimea with Kherson Oblast
cities <- rbind(citieskherson, citiescrimea, citieszaporizhia)


### keeping only city id, city name and region 
### Manually adding Simferopol because it is officialy not a part of Crimean administrative region 
cities[1450,1] <- 185 #adding ID
cities[1450,2] <- "Севастополь" #adding city name
cities[1450,3] <- NA #adding city name
cities[1450,4] <- "Крым" #adding region name

### Manually adding area "rayon" to cities where this is missing
cities[209,3] <- "Новокаховська міська рада"
cities[373,3] <- "Херсонська міська рада"
cities[416,3] <- "Алуштинська міська рада"
cities[426,3] <- "Балаклавський район"
cities[526,3] <- "Євпаторійська міська рада"
cities[575,3] <- "Севастопольська міська рада"
cities[593,3] <- "Керченська міська рада"
cities[886,3] <- "Сімферопольська міська рада"
cities[916,3] <- "Судацька міська рада"
cities[954,3] <- "Феодосійська міська рада"
cities[992,3] <- "Ялтинська міська рада"
cities[1130,3] <- "Запорізька міська рада"
cities[1444,3] <- "Енергодарська міська рада"

### creating a variable indicating whether the city is close to the border or not (0 = not at the border, 1= On the Kherson side 2= On the Crimea side)
cities$border <- 0
cities$border[cities$area == "Якимівський район"] <- 1
cities$border[cities$area == "Скадовський район"] <- 1
cities$border[cities$area == "Голопристанський район"] <- 1
cities$border[cities$area == "Каланчацький район"] <- 1
cities$border[cities$area == "Чаплинський район"] <- 1
cities$border[cities$area == "Новотроїцький район"] <- 1
cities$border[cities$area == "Генічеський район"] <- 1
cities$border[cities$area == "Чорноморський район"] <- 2
cities$border[cities$area == "Роздольненський район"] <- 2
cities$border[cities$area == "Красноперекопський район"] <- 2
cities$border[cities$area == "Джанкойський район"] <- 2
cities$border[cities$area == "Нижньогірський район"] <- 2
cities$border[cities$area == "Совєтський район"] <- 2


### keeping only cities from regions that are adjacent to the Crimea/Mainland Ukraine border
bordercities <- cities %>% filter(border >0) %>%  rename(city.id = id)

### creating a string that contains both place name and the rayon (adiministrative region)
bordercities$extended <- paste(bordercities$title,
                               bordercities$area,
                               sep = " ")


#### IMPORTANT In the next step, I have downloaded the coordinates in the cities that are 
####  located in "rayons" adjacent to the Crimea-Mainland border using Google's API through
#### the ggmap package in 2018. This can no longer be done free at the time of this writing.
####  I have therefore skipped the code in this file. Instead, I have  uploaded the 
####  coordinates originally acquired by me (loaded in the coordinates_full object below from my website). 
#### The "skipped" code originally used to download and process the coordiantes is available here: 
#### http://golovchenko.github.io/data/vk_censorship/downloading_coordinates.R
coordinates_full <- read.csv("http://golovchenko.github.io/data/vk_censorship/coordinates.txt")


###########################################################################
####Loading Ukraine Shapefile and computing distance from border ##########
###########################################################################

### IMPORTANT Download the shapefile from OCHA services in the link below and put it in your working diredtory:
### https://data.humdata.org/dataset/ukraine-administrative-boundaries-as-of-q2-2017

### loading shapefile for Ukraine
UA <- readOGR(dsn = ".", layer = "ukr_admbnda_adm1_q2_sspe_20171221")

### subsetting to relevant regions
subset_crimea <- UA[UA$ADM1_EN %in% c("Avtonomna Respublika Krym"),]
subset_Kherson_Zaporhizhia <- UA[UA$ADM1_EN %in% c("Khersonska", "Zaporizka"),]
subset_border <- UA[UA$ADM1_EN %in% c("Avtonomna Respublika Krym", "Khersonska", "Zaporizka"),]


### creating a variable indicating whether the city is in the Ukrainian mainald (border ==1) or in Crimea (border==2)
coordinates_full$border <- 0
coordinates_full$border[coordinates_full$region == "Херсонська область"] <- 1
coordinates_full$border[coordinates_full$region == "Запорізька область"] <- 1
coordinates_full$border[coordinates_full$region == "Крим "] <- 2
coordinates_full$border[coordinates_full$region == "Севастополь город"] <- 2
coordinates_full$border <- 0
coordinates_full$border[coordinates_full$area == "Якимівський район"] <- 1
coordinates_full$border[coordinates_full$area == "Скадовський район"] <- 1
coordinates_full$border[coordinates_full$area == "Голопристанський район"] <- 1
coordinates_full$border[coordinates_full$area == "Каланчацький район"] <- 1
coordinates_full$border[coordinates_full$area == "Чаплинський район"] <- 1
coordinates_full$border[coordinates_full$area == "Новотроїцький район"] <- 1
coordinates_full$border[coordinates_full$area == "Генічеський район"] <- 1
coordinates_full$border[coordinates_full$area == "Чорноморський район"] <- 2
coordinates_full$border[coordinates_full$area == "Роздольненський район"] <- 2
coordinates_full$border[coordinates_full$area == "Красноперекопський район"] <- 2
coordinates_full$border[coordinates_full$area == "Джанкойський район"] <- 2
coordinates_full$border[coordinates_full$area == "Нижньогірський район"] <- 2
coordinates_full$border[coordinates_full$area == "Совєтський район"] <- 2


### selecting only cities in Crimea
crimea <- coordinates_full %>% filter(border == 2) %>%rename(lat1 = lat, lon1 = lon)
### selecting only cities in Mainland Ukraine
mainland <- coordinates_full %>% filter(border == 1)%>%rename(lat1 = lat, lon1 = lon)

### storing coordinates for cities in Crimea in a separate dataframe
pts_crimea <- data.frame(x1 = crimea$lon1, x2 = crimea$lat1)

### storing coordinates for cities in mainland Ukraine in a separate dataframe
pts_mainland <- data.frame(x1 = mainland$lon1, x2 = mainland$lat1)

### computing the shortest distance between cities in Crimea and the Kherson/Zaporizhia border
dist.mat_crimea <- geosphere::dist2Line(p = pts_crimea, line = subset_Kherson_Zaporhizhia)

### computing the shortest distance between cities in mainland Ukraine and the Crimean border
dist.mat_mainland <- geosphere::dist2Line(p = pts_mainland, line = subset_crimea)

# bind results with original points
crimea_dist <- cbind(crimea, dist.mat_crimea)
mainland_dist <- cbind(mainland, dist.mat_mainland)

### merging the two dataframes with distances from city to border into one dataframe
dist <- rbind(crimea_dist, mainland_dist)

### Keeping only cities within 50 km from the border
dist_50 <- dist %>% filter(distance <= 50000)
dist_50$dist <- 50

### preparing GIS plot
locations_50 <- st_as_sf(dist_50, coords = c("lon1", "lat1"), crs = 4326)

### plotting the locations
mapview(locations_50)


#### Seeing which location is incorect
### city ids for cities that have to be requiried on google maps
target <- c(1512862,1500103,1500257,619,1433) # Джанкой, Дмитровка, Красноперекопск, Суворово, Пионерское

redo <- dist_50 %>% filter(ID %in% target)


####################################################################################################################
##Collecting metadata about users from the cities within the 50km range for the border##############################
####################################################################################################################


### making a function that identifies users in selected cities
searchallcities <- function(cities){
  random_numbers <- c(1:3)
  ids <- list()
  for (i in as.character(cities[1:length(cities)])){
    print(paste("processing", i, sep = " "))
    ids[[i]] = usersSearch(city = i, count = '1000')$items$id
    # waiting one second between hits
    Sys.sleep(sample(random_numbers, size = 1))
    cat(" done!\n")
  }
  ids
}


### testing the function
mainland_cities <- dist_50 %>% filter(border == 1)
crimean_cities <- dist_50 %>% filter(border == 2)
system.time(mainland_search <- searchallcities(mainland_cities$id))
system.time(crimean_search <- searchallcities(crimean_cities$id))
seed <- c(mainland_search, crimean_search)
seed <- unlist(seed)


### returning information about the users' city
u.info <- function(ids){
  vk.df <- getUsersExecute(ids, fields = "city")
  df <- data.frame(id = vk.df$id, city.id = vk.df$city$id)
  df
}


### Finding Vkontakte ids for cities in Crimea
# download a list with all cities in Crimea. 
citiescrimea<- databaseGetCities(country_id = 2, region_id = 1500001, count = 1000)
#write.csv(as.data.frame(citiescrimea$items), "citiescrimea.txt")
#keeping only city id
ids<- citiescrimea$items$id 
ids <- c(city.id, 185) # added this AFTER I finnished the crawl from cities in Crimea excluding Sevastopol
df.cities <- data.frame(city.id)
df.cities$crimea <-1

### making a function that keeps only users who reside in Crimea according to Vkontakte self-registered data
keep <- function(df) {
  a <- left_join(df, df.cities)
  a <- filter(a, border > 0) %>% select(id)
  return(a)
}

### creatinga function that returns friend ids for multiple users
crawl1 <- function(seed) {
  friends <- getFriendsFor(users_ids = seed) ### storing ids for the friends of the seed group
  info <- u.info(friends) #downloading information about friends
  local <- keep(info) # keeping only those living within 50km from the border
  local #outputing "local"
}

##### creating a crawler that does 'n' number of crawls and returns only the ids of the those living within 50 km from the border - exclusing the seed group
crawler <- function(seed, n) { #seed = seed group, n = number of crawls
  random <- c(60:120) # selecting a range of 60 to 120 seconds for the crawler to pause
  all <- crawl1(seed) #storing the ids in "all", which will be updated with each crawl
  all$crawled <- NA  #indicating that the users in "all" have not yet been crawled
  x <- 0 #creating a counter
  repeat {#iniates the repeat sequence, which will be carried on 'n' times
    x = x +1 
    print(x) #indicate how far the algorithm reach in the crawl (e.g. "2" means that the crawler is doing its 2nd crawl)
    if (x==n){ 
      break # stop the crawler once it has done n number of crawls, where n is specified by the user
    }
    notcrawled <- all %>% filter(is.na(crawled)) # selecting users that have not yet been crawled
    friends <-crawl1(notcrawled$id) #crawling the friends
    all$crawled <- 1 #indicating what users have been crawled
    friends <- left_join(friends, all) #joining the friends of friends with the compete dataset, to which users have already been crawled - keeping only the friends of friends
    friends <- friends %>% filter(is.na(crawled)) # selecting those who have not been crawled yet
    all <- rbind(friends, all) #adding friends of friends to the entire dataset 
    all <-  all %>% distinct(id, .keep_all = T) # removing duplicates
    Sys.sleep(sample(random, size =1)) # make a pause for a random number of seconds, where the shortest break is 60 seconds, and the longest is 120 seconds
  }
  all #print the entire dataset of users, excluding the seed group
}


## IMPORTANT Vkontakte's API often changes. In addition to this, it operates both with 
## explicit limit for download (in the developer documentation) and hidden limits that
## are not revealed in the documentation. If the function below does not return 
## output about users, try using lower number of crawls at a time (for example, n =1)
## in the function below and break down the process in multiple steps (using crawler()), 
## multiple times and merging the data instead of running the function only once in 
## one go 

### downloading all of the data through one round. 
border_users <- crawler(seed = seed, n = 6) # collected on the 17.08.2018 evening


### transforming the object with seed group ids into a dataframe
df.seed <- data.frame(id = seed)
df.seed$crawled <- NA

### merging seed group with the output from the crawler and keeping only unique values
all_users <- rbind(df.seed, border_users) %>% distinct(id, .keep_all = T)


### download meta data ab out the users
user_info<- getUsersExecute(users_ids = all_users$id, fields = "sex,bdate,city,country, last_seen, home_town,
                            followers_count, connections, verified, lists, universities, schools, occupation, relatives,
                            personal, interests, timezone") #collected on the 17th of August 2018


### pulling the relevant metadata into a dataframe
id <- user_info$id 
sex <- user_info$sex
bdate <-user_info$bdate
last_seen <- user_info$last_seen$time
date <-structure(last_seen, class = c("POSIXct", "POSIXt"))
date <- as.Date(date)
verified <- user_info$verified
city.id <- user_info$city$id
city.name <- user_info$city$title
deactivated <- user_info$deactivated
first_name <- user_info$first_name
last_name <- user_info$last_name
twitter <- user_info$twitter
facebook <- user_info$facebook
facebook_name <- user_info$facebook_name

df <- data.frame(id, first_name, last_name, sex,
                 bdate, date, verified, city.id,
                 city.name, deactivated, twitter, facebook, facebook_name)
df <- left_join(df, dist_50) %>%
  filter(!is.na(border), is.na(deactivated)) %>%
  distinct(id, .keep_all = T) #merging with geographic information and only keeping those that still live within the 50 km border ( a few people may change locaiton) 


### counting number of users in each city and sam
dist_50_count <- df %>% group_by(city.id) %>%
  mutate(count = n()) %>%
  distinct(city.id, .keep_all = T)

### counting number of users in each city that have logged in at least once since since 2014-06-06
dist_50_comparison <- df %>% group_by(city.id) %>%
  filter(date >="2014-06-06") %>%
  mutate(count = n()) %>%
  distinct(city.id, .keep_all = T)

### sampling 50 random cities and exporting csv. Each city is then manually looked up on VK, and the total number of users in the respective city written down
sample_cities <- dist_50_count %>% select(city.id, city.name) 
sample_cities$vksize <- NA
sample_cities$city.name <- NULL

### merging the city_level data with manually annotated population count and creating a variable which indicates what proportion of the online city population got captured in the crawl
dist_50_comparison <- left_join(dist_50_comparison, sample_cities) %>%
  mutate(vk_prop = count/vksize*100)
View(dist_50_comparison)

sum(dist_50_comparison$count)/sum(dist_50_comparison$vksize)*100


###########################################################################
#### Downloading information about the Vkontakte communities/groups #######
#### followed by the users#################################################
###########################################################################


### making a function that turn the list of groups for each user into a separate dataframe
community_df <- function(ids) {
  l <- length(ids[[1]])
  if (l > 0) {
    df<- data.frame(user_ids =names(ids), community_ids = ids)
    colnames(df)[2] <- "community_ids"
    df}
  else {
    df<- data.frame(user_ids = names(ids))
    df$community_ids <- NA
    df
  }
}

### downloading communities for all users
communities <- getGroupsForUsers(users = df$id)


## generating dataframe per user and merging the data frames
df_communities <- data.frame()
for (i in 1:length(communities)) {
  d <- community_df(communities[i])
  df_communities <- rbind(d, df_communities)
  df_communities
}


#### gettin friends for all the user within the 50 km border
friends <-getFriendsFor(users_ids = df$id)  #took 36 minutes
#system.time(edge <- getArbitraryNetwork(unique(unlist(friends)), format = "edgelist"))

#### making a function creates a friendship edgelist for the individual user
edgelist_df <- function(ids) {
  l <- length(ids[[1]])
  if (l > 0) {
    df<- data.frame(user_ids =names(ids), friends_ids = ids)
    colnames(df)[2] <- "friends_ids"
    df}
  else {
    df<- data.frame(user_ids = names(ids))
    df$friends_ids <- NA
    df
  }
}


#### creating an edgelist for each user
edge <- data.frame()
for (i in 1:length(friends)) {
  d <- edgelist_df(friends[i])
  edge <- rbind(d, edge)
  edge
}


### converting the columns from factor to numeric
edge$user_ids <- as.numeric(edge$user_ids)
edge$friends_ids <- as.numeric(edge$friends_ids)

#### combining bothing the searched users and their friends into one column, while keeping only unique ids
nodes_combined <-data.frame( id = unique(c(edge$user_ids, edge$friends_ids)))

#### keeping only the friends whose metadata has not already been collected
nodes_missing_meta <- left_join(nodes_combined, df) %>% filter(is.na(border))


### collecting metadata about the friends
system.time(friends_info<- getUsersExecute(users_ids = nodes_missing_meta$id, fields = "sex,bdate,city,country, last_seen, home_town,
                            followers_count, connections, verified, lists, universities, schools, occupation, relatives,
                            personal, interests, timezone")) #collected on the 19th of August 2018 # tool 180 minutes 


start.date<- as.Date(c("2017-05-15")) # the day the president signed the law
df$days<- difftime(df$date, start.date, units = c("days"))

### selecting "active users": those who have logged in up to 30 days prior to censorship or more
df.act <- df %>% filter(days >= -30)
df.act$days <- as.numeric(df.act$days)


### selecting active users on both sides of the border
#df.mainland <- df.act %>% filter(border == 1) 
#df.crimea <- df.act %>% filter(border == 2) 

### creating a dataframe of groups for both sides of the border
df_communities1 <- df_communities %>% rename(id = user_ids) # renameing the id variable

#unfactoring the id variable
df_communities1$id <- unfactor(df_communities1$id)

####creating a dataframe of groups for each side of the border
df.mainland.groups <- left_join(df_communities1, df.act, by = "id")%>%
  filter(border == 1)
df.crimea.groups <- left_join(df_communities1, df.act) %>%
  filter(border == 2)

### creating a dataframe of most popular groups in mainland Ukraine
df.mainland.groups.grouped <- df.mainland.groups %>%
  filter(!is.na(community_ids)) %>%
  group_by(community_ids) %>%
  mutate(group_count = n()) %>%
  distinct(community_ids, .keep_all = T) %>%
  arrange(-group_count)

### creating a dataframe of most popular groups in Crimea
df.crimea.groups.grouped <- df.crimea.groups %>%
  filter(!is.na(community_ids)) %>%
  group_by(community_ids) %>% 
  mutate(group_count = n()) %>%
  distinct(community_ids, .keep_all = T) %>%
  arrange(-group_count)

summainland <- sum(df.mainland.groups.grouped$group_count)
sum1000 <- sum(df.mainland.groups.grouped$group_count[1:10000])
sum1000/summainland*100

sumcrimea <- sum(df.crimea.groups.grouped$group_count)
sum1000 <-  sum(df.crimea.groups.grouped$group_count[1:5000])
sum1000/sumcrimea*100



################################################################################
###Downloading group info for top 10000 users on each side of the border########
################################################################################
### most of the function below are for interacting with the Vkontakte API and are
### from the from https://github.com/Dementiy/vkR . They had to reiterated (and corrrected)
### here due to errors in the VkR package. 

### creating a querry builder
queryBuilder <- function(method_name, ...) {
  query <- paste("https://api.vk.com/method/", method_name, "?", sep = "")
  arguments <- sapply(substitute(list(...))[-1], deparse)
  arg_names <- names(arguments)
  for (arg_pos in seq(length(arguments))) {
    if (arg_names[arg_pos] != "") {
      if (is.character(arguments[arg_pos])) {
        #arg_value <- gsub("\"", "", arguments[arg_pos])
        arg_value <- list(...)[arg_names[arg_pos]]
      } else {
        # ???
        arg_value <- arguments[arg_pos]
      }
      query <- paste(query, ifelse(arg_value != "", paste("&", arg_names[arg_pos], "=", arg_value, sep = ""), ""), sep = "")
    }
  }
  query <- paste(query, '&access_token=', getAccessToken(), sep = "")
  query
}

###
if (!exists(".vkr")) {
  .vkr <- new.env()
  .vkr$access_token <- NULL
  .vkr$api_version <- '5.73'
  .vkr$me <- 0L
  .vkr$last_request_time <- 0
  .vkr$num_requests <- 0
  .vkr$max_requests <- 3
  
  # Database variables
  .vkr$db_name <- 'vkR_projects'
  .vkr$db_active <- NULL
  .vkr$db_meta_name <- 'meta_collection'
  .vkr$db_metadata <- NULL
  
  # Handling connection errors
  .vkr$timeout <- 3
  .vkr$max_repeats <- 3
  .vkr$repeats_counter <- 0
}



##
getAPIVersion <- function() {
  .vkr$api_version
}


##
try_handle_error <- function(response) {
  tryCatch(
    vk_stop(message = response$error$error_msg,
            error_code = response$error$error_code),
    vk_error14 = function(e) {
      params <- handle_captcha(response$error)
      return(repeat_last_query(params = params, n = 6))
    }, vk_error17 = function(e) {
      handle_validation(response$error)
      return(repeat_last_query(n = 6))
    }, vk_error6  = function(e) {
      request_delay()
      return(repeat_last_query(n = 6))
    }
  )
}

## creating a function to get information about groups
getGroupsById <- function(group_id='', sort='', offset='', count='', fields='', filter='', v=getAPIVersion()) {
  query <- queryBuilder('groups.getById',
                        group_ids = group_id,
                        sort = sort,
                        offset = offset,
                        count = count,
                        fields = fields,
                        filter = filter,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(query)
  
  if (has_error(response))
    return(try_handle_error(response))
  
  response$response
}


###
request_delay <- function()
{
  start_time <- Sys.time()
  taken_time <- start_time - .vkr$last_request_time
  if (taken_time <= 1.0 & .vkr$num_requests >= .vkr$max_requests) {
    Sys.sleep(1.0 - taken_time)
  }
  .vkr$num_requests <- ifelse(.vkr$num_requests < 3, .vkr$num_requests + 1, 1)
  .vkr$last_request_time <- Sys.time()
}

###
has_error <- function(response) {
  return(ifelse(!is.null(response$error), response$error$error_code, 0))
}
####
vk_stop <- function(message = "", call = sys.call(), error_code = "") {
  cond <- structure(list(message = message, call = call),
                    class = c(paste0("vk_error", error_code), "error", "condition"))
  stop(cond)
}




### creating a function that creates a df with group info, for each group
group_info <- function(group_list){
  df <- data.frame(community_ids = group_list[["id"]],
                   screen_name = group_list[["screen_name"]],
                   name = group_list[["name"]],
                   type = group_list[["type"]],
                   is_closed = group_list[["is_closed"]])
  df
}

### Making a function that finds all users in a city (up to aprox 100,000 users)
getGroupsByIdMultiple <- function(groups) {
  all <- data.frame()
  for (i in 1:length(groups)){
    df <- group_info(getGroupsById(groups[i]))
    Sys.sleep(0.5)
    print(paste(i, "processing group ID", groups[i], sep = " "))
    all <-rbind(df, all)
  }
  all
}


topmain <- getGroupsByIdMultiple(df.mainland.groups.grouped$community_ids[1:5000]) #began 22.08.2018 08:57
system.time(topmain1 <- getGroupsByIdMultiple(df.mainland.groups.grouped$community_ids[5001:10000])) #began 22.08.2018 08:57

### exporting the groups and manually annotating them (see the details about data collection in the manuscript)
topmain2 <- rbind(topmain, topmain1)
topmain_exp <- topmain2 %>% select(community_ids, name)
topmain_exp$name_category <- 0 
topmain_exp$allignment <- NA
write.csv(topmain_exp, "topgroups_mainland1.csv", row.names = F)

### loading the manually annotated data
annotated_mainland <- read.csv("topgroups_mainland1_annotated.csv", stringsAsFactors = F)
### Selecting dataframe only with partisan users (one row per user->group link)
annotated_mainland1 <- left_join(df.mainland.groups, annotated_mainland) %>%
  filter(!is.na(allignment), allignment <=2) 

### creat a variable indicating whether the edge leads to a pro-Russian group
annotated_mainland1$ru <- 0
annotated_mainland1$ru[annotated_mainland1$allignment == 1] <- 1
### creat a variable indicating whether the edge leads to a pro-Ukrainian group
annotated_mainland1$ua <- 0
annotated_mainland1$ua[annotated_mainland1$allignment == 2] <- 1

annotated_mainland1 <- annotated_mainland1 %>%
  group_by(id) %>%
  mutate(ru_sum = sum(ru), ua_sum = sum(ua)) %>%
  distinct(id, .keep_all = T)

pro_rus <- annotated_mainland1 %>% 
  filter(ru_sum > 0, ua_sum == 0)

pro_ukr <- annotated_mainland1 %>% 
  filter(ua_sum > 0, ru_sum == 0)
nrow(pro_rus)
nrow(pro_ukr)


topcrimea <- df.crimea.groups.grouped[1:10000,]
topcrimea <- left_join(topcrimea, annotated_mainland)
topcrimea_to_annotate <- topcrimea %>%
  filter(is.na(name_category)) 
topcrimea_to_annotate <- getGroupsByIdMultiple(topcrimea_to_annotate$community_ids)
topcrimea_to_annotate1 <- topcrimea_to_annotate %>% select(community_ids, name)
topcrimea_to_annotate1$name_category <- 0 
topcrimea_to_annotate1$allignment <- NA


### keeping only groups from Crimea where the affiliation is already known 
topcrimea1 <- topcrimea %>% filter(!is.na(name_category)) %>% 
  select(community_ids, name, name_category, allignment)
# converting to simple dataframe
topcrimea1 <- as.data.frame(topcrimea1)

### loading the remaining groups that have been manually annotated 
topcrimea2 <- read.csv("topgroups_crimea.csv", stringsAsFactors = F)

### merging the two datasets with groups
annotated_crimea <- rbind(topcrimea1, topcrimea2)


### Selecting dataframe only with partisan users (one row per user->group link)
annotated_crimea1 <- left_join(df.crimea.groups, annotated_crimea) %>%
  filter(!is.na(allignment), allignment <=2) 

### creat a variable indicating whether the edge leads to a pro-Russian group
annotated_crimea1$ru <- 0
annotated_crimea1$ru[annotated_crimea1$allignment == 1] <- 1
### creat a variable indicating whether the edge leads to a pro-Ukrainian group
annotated_crimea1$ua <- 0
annotated_crimea1$ua[annotated_crimea1$allignment == 2] <- 1

annotated_crimea1 <- annotated_crimea1 %>%
  group_by(id) %>%
  mutate(ru_sum = sum(ru), ua_sum = sum(ua)) %>%
  distinct(id, .keep_all = T)

pro_rus_crimea <- annotated_crimea1 %>% 
  filter(ru_sum > 0, ua_sum == 0)

pro_ukr_crimea <- annotated_crimea1 %>% 
  filter(ua_sum > 0, ru_sum == 0)
nrow(pro_rus_crimea)
nrow(pro_ukr_crimea)


###########################################################################
####### downloading wall posts from the sub-sample of users################
###########################################################################

### selecting users from Mainland and Crimea
df.mainland <- df.act %>% filter(border == 1) 
df.crimea <- df.act %>% filter(border == 2) 


########### Creating functions needed to download the wallposts############

### function that returns a list with metadata for each wall post for each user
getWalls <- function(user_ids) {
  random <- c(1:1)
  walls <- c()
  for (i in 1:length(user_ids)) {
    tryCatch({
      print(paste(i, "processing user ID", user_ids[i], sep = " "))
      walls[[i]] <- getWallExecute(owner_id = user_ids[i],
                                   filter = "owner",
                                   count = 0)$posts
      Sys.sleep(sample(random, size = 1))
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})}
  walls
}


### a function that transforms list with wall metadata for a single user into a dataframe while keeping only selected variables
wall_meta <- function(list) {
  if (length(list) > 0) #if the list is not empty, fill in the variables
    df <- as.data.frame(list[[1]])
  df1 <- data.frame(id = df$id,
                    from_id =df$from_id,
                    owner_id = df$owner_id,
                    date = df$date,
                    text = df$text)
  if (length(list) == 0) #if the list is empty, return a dataframe with the same variables but with empty rows
    df1 <- data.frame(id = NA,
                      from_id = NA,
                      owner_id =NA, 
                      date = NA,
                      text = NA)
  
  df1
}


### making the same as above but in a for loop
wall_df <- function(list1) {
  df <- data.frame()
  for (i in 1:length(list1)) {
    wall<- wall_meta(list1[i])
    df <- rbind(wall, df)
  }
  df
}


################ Downloading wallposts from Mainland Ukraine ######################
## IMPORTANT Vkontakte's API often changes. In addition to this, it operates both with 
## explicit limit for download (in the developer documentation) and hidden limits that
## are not revealed in the documentation. If the functions below for downloading wallposts
## do not return output about users, try downloading data about a lower number of users at 
## a time and then merging the files together. This was my strategy, as you will see below. 
## I had to break down the data collection in very different data pieces instead running
## all of it through one function, because I had to have breaks for an (seemingly arbitrary)
## number of minutes/hours before I could regain access to the relevant data through the API
## I believe the API has been complicated in this way in order to limit access to the data
## in away that discourages app developers from overloading the API with requests. 


#### see previous code on how "pro_rus was created
rus_walls1 <- getWalls(pro_rus$id[1:400])
rus_walls2 <- getWalls(pro_rus$id[401:597]) #Began  downloading pro-rus users in Ukraine on the 24th,finnished downloading on the 27th 
rus_walls <- c(rus_walls1, rus_walls2)


# randomly sampling 600 pro ukrainian users on the mainland side of the border
set.seed(1312)
pro_ukr_r <-  sample(pro_ukr$id, size = 600)
pro_ukr_r_info <- user_info(pro_ukr_r)
ukr_walls1 <- getWalls(pro_ukr_r[1:300])
ukr_walls2 <- getWalls(pro_ukr_r[301:600])
ukr_walls <- c(ukr_walls1, ukr_walls2)

###sampling 600 random users from Mainland
set.seed(131234)
mainland_r <- df.act %>%
  filter(border == 1, !is.na(id)) %>%
  sample_n(size=600) 

### updating information about random users in order to remove deactivated users
mainland_r_info<- as.data.frame(getUsersExecute(users_ids = mainland_r$id))
mainland_r_info <- mainland_r_info %>% filter(is.na(deactivated)) 
# download walls from 600 random users on the mainland side of the border
ran_walls1 <- getWalls(mainland_r_info$id[1:200])
ran_walls2 <- getWalls(mainland_r_info$id[401:598])
ran_walls3.1 <- getWalls(mainland_r_info$id[201:240])
ran_walls3.2 <- getWalls(mainland_r_info$id[242:400]) # skipping bad row




### combinging the random users into one list
### merging the different files with wallpostsfrom random users from mainland Ukraine
ran_walls <- c(ran_walls1, ran_walls2, ran_walls3, ran_walls3.1,
               ran_walls3.2)



##############################################################################################################
#######################Downloading wall posts from Crimea##########################################################
##############################################################################################################
###sampling 600 random users from mainland
set.seed(13634)
crimea_r <- df.act %>%
  filter(border == 2, !is.na(id)) %>%
  sample_n(size=600) #setting sample size to 700, because of the most likely have changed their privacy setting since the data was first collected


### updating information about random users in order to remove deactivated users
crimea_r_info<- as.data.frame(getUsersExecute(users_ids = crimea_r$id))
crimea_r_info <- crimea_r_info %>% filter(is.na(deactivated)) 


### downloading the wall posts from randomly selected users in Crimea
ran_crimea_walls1 <- getWalls(crimea_r_info$id[1:200])
ran_crimea_walls2 <- getWalls(crimea_r_info$id[201:400])
ran_crimea_walls3 <- getWalls(crimea_r_info$id[401:600])
### sampling pro-ukrainian users on the Crimean side of the border
set.seed(5612)
pro_ukr_crimea_r <-  sample(pro_ukr_crimea$id, size = 700)
### downloading pro-Ukrainan walls
ukr_crimea_walls1 <- getWalls(pro_ukr_crimea_r[1:200])
ukr_crimea_walls2 <- getWalls(pro_ukr_crimea_r[201:400])
ukr_crimea_walls3 <- getWalls(pro_ukr_crimea_r[401:600])

### sampling pro-ukrainian users on the Crimean side of the border
pro_rus_crimea_r <-  sample(pro_rus_crimea$id, size = 700)
### downloading pro-Ukrainan walls
rus_crimea_walls1 <- getWalls(pro_rus_crimea_r[1:200])
rus_crimea_walls2 <- getWalls(pro_rus_crimea_r[201:400])
rus_crimea_walls3 <- getWalls(pro_rus_crimea_r[401:600])
rus_crimea_walls4 <- getWalls(pro_rus_crimea_r[601:700])

#### leftovers 
ukr_crimea_walls1 <- getWalls(pro_ukr_crimea_r[601:700])

set.seed(12624)
crimea_r_100 <- df.act %>%
  filter(border == 2, !is.na(id)) %>%
  sample_n(size=100)

ran_crimea_walls4 <- getWalls(crimea_r_100$id)



###########################################################################
############## creating functions that transform the wallposts data #######
############## into a dataframe ###########################################
###########################################################################


### a function that transforms list with wall metadata for a single user into a dataframe while keeping only selected variables
wall_meta <- function(list) {
  if (length(list) > 0) #if the list is not empty, fill in the variables
    df <- as.data.frame(list[[1]])
  df1 <- data.frame(id = df$id,
                    from_id =df$from_id,
                    owner_id = df$owner_id,
                    date = df$date,
                    text = df$text)
  if (length(list) == 0) #if the list is empty, return a dataframe with the same variables but with empty rows
    df1 <- data.frame(id = NA,
                      from_id = NA,
                      owner_id =NA, 
                      date = NA,
                      text = NA)
  
  df1
}


### making the same as above but in a for loop
wall_df <- function(list1) {
  df <- data.frame()
  for (i in 1:length(list1)) {
    wall<- wall_meta(list1[i])
    df <- rbind(wall, df)
  }
  df
}

###combining lists with wall post metadata
#random users from Mainland
ran_walls <- c(ran_walls1, ran_walls2, ran_walls3.1,
               ran_walls3.2)

#pro-Russian users from Mainland
rus_walls <- c(rus_walls1, rus_walls2)
#pro-Ukrainian users from Mainalnd
ukr_walls <- c(ukr_walls1, ukr_walls2)

#random users from Crimea
ran_crimea_walls <- c(ran_crimea_walls1, ran_crimea_walls2,
                      ran_crimea_walls3, ran_crimea_walls4)
#pro-Russian users from Crimea
rus_crimea_walls <- c(rus_crimea_walls1, rus_crimea_walls2, 
                      rus_crimea_walls3, rus_crimea_walls4)
#pro-Ukrainian users from Crimea
## fixing a mistake, what would have been crimea_walls4 was saved as crimea_walls1, overwriting the original crimea_walls1

ukr_crimea_walls4 <- ukr_crimea_walls1
ukr_crimea_walls <- c(ukr_crimea_walls1, ukr_crimea_walls2,
                      ukr_crimea_walls3, ukr_crimea_walls4)


##########################################################################
############Transforming the lists into a dataframe ######################
##########################################################################

### creating dataframe for random_users in Mainland
ran_mainland_df <- wall_df(ran_walls)

### creating dataframe for pro-Russian users in Mainland
rus_mainland_df <- wall_df(rus_walls)

### creating dataframe for pro-Ukrainian users in Mainland
ukr_mainland_df <- wall_df(ukr_walls)

### creating dataframe for random_users in Crimea
ran_crimea_df <- wall_df(ran_crimea_walls)

### creating dataframe for pro-Russian users in Crimea
rus_crimea_df <- wall_df(rus_crimea_walls)

### creating dataframe for pro-Ukrainian users in Crimea
ukr_crimea_df <- wall_df(ukr_crimea_walls)



### indicating wether the posts were written by pro-Ukrainian or pro-Russian user
rus_mainland_df$pol <- "Pro-Russian"
ukr_mainland_df$pol <- "Pro-Ukrainian"
rus_crimea_df$pol <- "Pro-Russian"
ukr_crimea_df$pol <- "Pro-Ukrainian"

### indicating wether the posts are written by users from Crimea or mainland Ukraine
rus_mainland_df$border <- "Mainland Ukraine"
ukr_mainland_df$border <- "Mainland Ukraine"
rus_crimea_df$border <- "Crimea"
ukr_crimea_df$border <- "Crimea"

### merging the dataframes
walls <- rbind(rus_mainland_df, ukr_mainland_df,
               rus_crimea_df, ukr_crimea_df)


### creating a numeric dummy variable for each of the political category
walls$ukr <- 0
walls$ukr[walls$pol == "Pro-Ukrainian"] <- 1
walls$rus <- 0
walls$rus[walls$pol == "Pro-Russian"] <- 1

#### Creating a variable indicating only the year and month
walls$date1 <-structure(walls$date, class = c("POSIXct", "POSIXt"))
walls$date1  <- as.Date(walls$date1)
walls$month <- month(walls$date1)
walls$year <- year(walls$date1)
walls$time <- as.Date(sprintf('%02d-%02d', walls$year, walls$month))
### count days after the censorship
start.date<- as.Date(c("2017-05-15")) # the day the president signed the law
walls$days<- difftime(walls$date1, start.date, units = c("days"))







####################################################################################################
#################### Downloading the relevant network friendship data ##############################
####################################################################################################

####a function that makes an edgelist (showing who the user is friends with) for a single user
edgelist_df <- function(ids) {
  l <- length(ids[[1]])
  if (l > 0) {
    df<- data.frame(user_ids =names(ids), friends_ids = ids)
    colnames(df)[2] <- "friends_ids"
    df}
  else {
    df<- data.frame(user_ids = names(ids))
    df$friends_ids <- NA
    df
  }
}


#### creating an edgelist for emultiple users
edge <- data.frame()
for (i in 1:109191) {
  d <- edgelist_df(friends[i])
  edge <- rbind(d, edge)
  edge
}

### renaming a renaming the variable in order to be able to merge the data set later on
edge <- edge %>%
  rename(owner_id = user_ids) 



###################################################################################################
################Examing posting activity and friendship network ###################################
###################################################################################################

### filtering out users who have posted more than 20.000 posts
walls.clean <- walls %>%
  group_by(owner_id) %>%
  mutate(sum_total = n(),
         min_days =min(days1),
         max_days = max(days1),
         total_days = max_days -min_days +1,
         mean_posts = sum_total/total_days) %>%
  filter(mean_posts <= 5)

### keeping only randomly sampled users (whose wallposts we have) in the edgelist 
##keeping only unique users with wallposts
walls.users <- as.data.frame(walls.clean) %>%
  distinct(owner_id, .keep_all = T)

## formatting the id variable 
#walls.users$owner_id <- as.numeric(walls.users$owner_id)

## creating a variable indicating that the user is part of the wallpost sample
walls.users$sample <-1



## adding metadata about the users with wall posts to the edgelist
walls.edge <- left_join(edge, walls.users) 

## filtering out friendship networks for users outside of the wall sample
walls.edge.1 <- walls.edge %>%
  filter(sample ==1)


### reassembling the dataframe with all of the users within th 50 km, this time also with the country variable
country<- user_info$country$id

df_country <- data.frame(id,  sex,
                         bdate, date, verified, city.id,
                         city.name, country)

df_country <- left_join(df_country, dist_50) %>%
  filter(!is.na(border), is.na(deactivated)) %>%
  distinct(id, .keep_all = T)


### creating a df with complete metadata for users on both sides of the border and their friends
df_b <- df_country %>% select(id, sex, city.id, city.name, country, border)
df_b$country <- NA

friends_info.1 <- data.frame(id = friends_info$id,
                             sex = friends_info$sex,
                             city.id = friends_info$city$id,
                             city.name = friends_info$city$id,
                             country = friends_info$country$id)


friends_info.1$border <- NA


### merging the metadata for users on both sides of the boarder and their friends
meta_complete <- rbind(df_b, friends_info.1)


### keepin only unique users and renaming the id variable before merging
meta_complete <- meta_complete %>%
  distinct(id, .keep_all = T) %>% #this line serves as an extra control to ensure that there are no duplicates - 
  rename(friends_ids = id)
### making sure that users from the border sample are categorized as Ukrainian if they are from Mainland Ukraine
meta_complete$country[meta_complete$border == 1] <- 2
meta_complete$border <- NULL


### removing the variables in meta_complete that also occur in walls.edge.1 before merging the two
#walls.edge.1$date <- NULL
#walls.edge.1$border <- NULL
### adding the metadata about the friends to the edgelist for the sampled users
walls.edge.2 <- left_join(walls.edge.1, meta_complete)

## filtering out rows where the city is uknoqn 
walls.edge.2 <- walls.edge.2 %>%
  filter(!is.na(city.id))


### adding the crimea variable into the edgelist
walls.edge.2 <- left_join(walls.edge.2, df.cities)

walls.edge.2$country[walls.edge.2$crimea == 1] <- 10000 # assigning Crimea as a separate "country" with code 10000



### computing what proportion of the users friends are from Russia
#creating a variable indicating wether the friendship tie links to someone in Russia
walls.edge.2$rus_friends <- 0
walls.edge.2$rus_friends[walls.edge.2$country == 1] <- 1

#creating a variable indicating wether the friendship tie links to someone in Ukraine
walls.edge.2$ukr_friends <- 0
walls.edge.2$ukr_friends[walls.edge.2$country == 2] <- 1


# assignng variable for crimean friends
walls.edge.2$crimea_friends <- 0
walls.edge.2$crimea_friends[walls.edge.2$country == 10000] <- 1


### adding in information on which side of the boarder does the user in the wallpost sample live
walls.edge.2$border <- NULL #removing the corrupt border variable
border <- walls.users %>% select(owner_id, border) # creating a data frame where each row is a unique user from the wall post sample with only two variables
walls.edge.2 <- left_join(walls.edge.2, border)  

### computing number of friends, and proportion of friendship ties linking to Russia
walls.users.1 <- walls.edge.2 %>%
  group_by(owner_id) %>%  #grouping by users in the wallpost sample
  mutate(sum_friends = n(),
         sum_rus_friends = sum(rus_friends),
         sum_crimea_friends = sum(crimea_friends),
         sum_ukr_friends = sum(ukr_friends),
         prop_rus_friends = sum_rus_friends/sum_friends,
         prop_crimea_friends = sum_crimea_friends/sum_friends,
         prop_ukr_friends = sum_ukr_friends/sum_friends) %>%
  distinct(owner_id, .keep_all = T)


### creating a dummy variable indicating wether the proportion of friends living in Russia is above the media (in mainland Ukraine) or not
walls.users.1$median <- "Up to median proportion"
walls.users.1$median[walls.users.1$prop_rus_friends > 0.05581] <-"Above median proportion" #IMPORTANT replace the number with the media value for your respective data, if you have redownlaoded the data
walls.users.1$median.1 <- 0
walls.users.1$median.1[walls.users.1$prop_rus_friends > 0.05581] <-1 #IMPORTANT update the number if you have redownlaoded the data



### creating a dummy variable indicating wether the proportion of friends living in Russia is above the media or not
walls.users.1$half <- "Up to half of friends live in Russia"
walls.users.1$half[walls.users.1$prop_rus_friends > 0.5] <-"More than half of friends live in Russia"

walls.users.1$most <- "Up to 75 percent of friends live in Russia"
walls.users.1$most[walls.users.1$prop_rus_friends > 0.75] <-"More than 75 percent of friends live in Russia"


### creating a dummy variable indicating wether the proportion of friends living in Ukraine 
walls.users.1$half_ukr <- "Up to half of friends live in Ukraine"
walls.users.1$half_ukr[walls.users.1$prop_ukr_friends > 0.5] <-"More than half of friends live in Ukraine"

walls.users.1$half_ukr.1 <- 0
walls.users.1$half_ukr.1[walls.users.1$prop_ukr_friends > 0.5] <-1

walls.users.1$most_ukr <- "Up to 75 percent of friends live in Ukraine"
walls.users.1$most_ukr[walls.users.1$prop_rus_friends > 0.75] <-"More than 75 percent of friends live in Ukraine"

### creating a dummy variable indicating wether the proportion of friends living in Crimea
walls.users.1$half_crimea <- "Up to half of friends live in Crimea"
walls.users.1$half_crimea[walls.users.1$prop_crimea_friends > 0.5] <-"More than half of friends live in Crimea"

walls.users.1$half_crimea.1 <- 0
walls.users.1$half_crimea.1[walls.users.1$prop_crimea_friends > 0.5] <-1


walls.users.1$most_crimea <- "Up to 75 percent of friends live in Crimea"
walls.users.1$most_crimea[walls.users.1$prop_crimea_friends > 0.75] <-"More than 75 percent of friends live in Crimea"



walls.users.1$median_crimea <- "Up to median proportion"
walls.users.1$median_crimea[walls.users.1$prop_crimea_friends > 0.02239] <-"Above median proportion" #IMPORTANT update the number if you have redownlaoded the data




### selecting only the nessecary variables
walls.users.2 <- walls.users.1 %>%
  select(owner_id, sum_friends,
         sum_rus_friends, prop_rus_friends,
         prop_crimea_friends, prop_ukr_friends,
         median, median.1,half_ukr.1, half_crimea.1,
         median_crimea,most, sex)


###################################################################################################
################ Downloading wall reposts in order to compute   ###################################
################ ties strength between users instead of a binary###################################
################ friend/non-friend distinction ####################################################
###################################################################################################


### downloading a list of cities in Crimea
# Accesing Vkontakte's API
vkOAuth(client_id = 'INSERT YOUR ID', 'groups, walls, friends')
setAccessToken(access_token = 'INSERT YOUR ACCESS TOKEN ') ### see "1_downloading_data.R"

# download list of cities in Crimea from Vkontakte
citiescrimea<- databaseGetCities(country_id = 2, region_id = 1500001, count = 1000)


### indicating in the user meta data whether the user is from Crimea or not
meta_complete <- left_join(meta_complete, df.cities)

meta_complete_short <- meta_complete %>%
  rename(from_id = friends_ids,
         city.id.post = city.id, 
         city.name.post = city.name,
         country.post = country, 
         sex.post = sex) %>% # renaming the id variable in order to match the data frame with "walls"; renaming the other variables to indicate that they are titled to the Reposted users
  filter(!is.na(city.id.post))

### assigning Crimea as a separate "country" with code 10000
meta_complete_short$country.post[meta_complete_short$crimea == 1] <- 10000 


### adding metadata about the reposted users to the wall posts data frame
walls.other.meta <- left_join(walls.other, meta_complete_short)


### indicating whether the reposts person lives in Ukraine, Russia or Crimea
walls.other.meta$repost_origin <- NA
walls.other.meta$repost_origin[walls.other.meta$country.post == 1] <- "Russia"
walls.other.meta$repost_origin[walls.other.meta$country.post == 2] <- "Ukraine"
walls.other.meta$repost_origin[walls.other.meta$crimea == 1] <- "Crimea"
table(walls.other.meta$repost_origin)



###################################################################################################
############## Computing number of posts from Russia and user activity#############################
###################################################################################################

### Creating a binary variale indicating whether respective the post originated from Russia, Ukraine or Crimea
walls.other.meta$post_ru <- 0
walls.other.meta$post_ru[walls.other.meta$repost_origin == "Russia"] <- 1

walls.other.meta$post_ua <- 0
walls.other.meta$post_ua[walls.other.meta$repost_origin == "Ukraine"] <- 1

walls.other.meta$post_crim <- 0
walls.other.meta$post_crim[walls.other.meta$repost_origin == "Crimea"] <- 1


### counting number of users that have posted on the respective walls
user_post <- walls.other.meta %>%
  group_by(owner_id) %>%#grouping by users in the wallpost sample
  filter(!is.na(city.id.post)) %>% # keeping only posts from users whose city we know
  mutate(sum_friends_post_raw = n(), #number of cross-wall posts on the respective wall
         sum_rus_friends_post_raw = sum(post_ru), 
         sum_ukr_friends_post_raw = sum(post_ua), 
         sum_crimea_friends_post_raw = sum(post_crim),
         prop_rus_friends_post_raw = sum_rus_friends_post_raw/sum_friends_post_raw,
         prop_ukr_friends_post_raw = sum_ukr_friends_post_raw/sum_friends_post_raw,
         prop_crimea_friends_post_raw = sum_crimea_friends_post_raw/sum_friends_post_raw) %>%
  distinct(from_id, .keep_all = T) %>% # for each user wall post, keeping only unique posting friends
  mutate(sum_friends_post = n(), #number of friends that posted on the respective wall
         sum_rus_friends_post = sum(post_ru), 
         sum_ukr_friends_post = sum(post_ua), 
         sum_crimea_friends_post = sum(post_crim),
         prop_rus_friends_post = sum_rus_friends_post/sum_friends_post,
         prop_ukr_friends_post = sum_ukr_friends_post/sum_friends_post,
         prop_crimea_friends_post = sum_crimea_friends_post/sum_friends_post) %>%
  distinct(owner_id, .keep_all = T)

###adding the user metada above to the wall posts dataset
# selecting the relevant variales
users_post_short <- user_post %>% 
  select(owner_id, sum_friends_post, 
         sum_rus_friends_post, sum_ukr_friends_post,
         sum_crimea_friends_post, prop_rus_friends_post,
         prop_ukr_friends_post,prop_crimea_friends_post,
         sum_friends_post_raw, 
         sum_rus_friends_post_raw, sum_ukr_friends_post_raw,
         sum_crimea_friends_post_raw, prop_rus_friends_post_raw,
         prop_ukr_friends_post_raw,prop_crimea_friends_post_raw)

walls.other.meta.1 <- left_join(walls.other.meta, users_post_short) #joining metadata for users with wall posts data


### creating a variable indicating whether the user has at least one friend posting from the region in question nor not
walls.other.meta.1$user_ties_rus <- "No strong ties in Russia"
walls.other.meta.1$user_ties_rus[walls.other.meta.1$sum_rus_friends_post > 0] <- "At least 1 strong tie in Russia"


walls.other.meta.1$user_ties_ua <- "No strong ties in Ukraine"
walls.other.meta.1$user_ties_ua[walls.other.meta.1$sum_ukr_friends_post > 0] <- "At least 1 strong tie in Ukraine"


walls.other.meta.1$user_ties_crim <- "No strong ties in Crimea"
walls.other.meta.1$user_ties_crim[walls.other.meta.1$sum_crimea_friends_post > 0] <- "At least 1 strong tie in Crimea"

walls.other.meta.1$user_ties_rus_raw <- "Less than 5 posts from Russia"
walls.other.meta.1$user_ties_rus_raw[walls.other.meta.1$sum_rus_friends_post > 4] <- "At least 5 posts from Russia"

### creating a data set with unique user ties
unique.users.ties <- walls.other.meta.1 %>%
  distinct(owner_id, .keep_all = T) %>% ### keeping only unique users
  select(owner_id,
         sum_rus_friends_post,
         sum_ukr_friends_post,
         sum_crimea_friends_post,
         prop_rus_friends_post,
         prop_ukr_friends_post,
         prop_crimea_friends_post,
         sum_friends_post_raw, 
         sum_rus_friends_post_raw,
         sum_ukr_friends_post_raw,
         sum_crimea_friends_post_raw,
         prop_rus_friends_post_raw,
         prop_ukr_friends_post_raw,
         prop_crimea_friends_post_raw, 
         user_ties_rus,
         user_ties_ua,
         user_ties_crim,
         user_ties_rus_raw
  )## selecting the user id and variables on tie strength

### storing the large data set for later (see 3_appendix.R) in the working directory
write.csv(unique.users.ties, "unique_user_ties.csv", row.names = F)
