#==============================================================================
# collect-data.R
# Purpose: illustrate how tweets can be collected using tweet IDs (provided in
# replication files)
# Author: Pablo Barbera
#==============================================================================

#### INSTALLING SMaPP-LAB R PACKAGES FROM GITHUB ####
library(devtools)
install_github("SMAPPNYU/smappR")

#### INSTALLING ADDITIONAL PACKAGES ####
install.packages("streamR")

### REGISTERING OAUTH TOKEN ###

## Step 1: go to apps.twitter.com and sign in
## Step 2: click on "Create New App"
## Step 3: fill name, description, and website (it can be anything, even google.com).
##         (Leave callback ULR empty)
## Step 4: Agree to user conditions and enter captcha.
## Step 5: copy consumer key and consumer secret and paste below

library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "XXXXXXXXXXXX"
consumerSecret <- "YYYYYYYYYYYYYYYYYYY"
my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
  consumerSecret=consumerSecret, requestURL=requestURL,
  accessURL=accessURL, authURL=authURL)

## run this line and go to the URL that appears on screen
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

## Setting working folder
## From windows machine usually this works
# setwd("H:\\credentials\\twitter")
## From Mac computer, something like...
setwd("~/credentials/twitter/")

## now you can save oauth token for use in future sessions with twitteR or streamR
save(my_oauth, file="my_oauth")

#### COLLECTING TWEETS BY ID ####

library(smappR)

## Example: reading first 100 tweets
ids <- scan("occupy.txt", n=100, what="character")

## downloading statuses
getStatuses(ids=ids, filename='occupy-tweets.json',
    oauth_folder = "~/credentials/twitter")

## reading tweets in R
library(streamR)
tweets <- parseTweets("minimum-wage-tweets.json")

# (total of tweets will be lower because of deleted tweets, deactivated
# accounts, etc.)

# It is possible to work with tweets in JSON format, but for large 
# datasets we recommend hosting them in a MongoDB database.
# Here we show how one can dump tweets into this format.
# (We assume mongoDB is installed and running)
library(smappR)
tweetsToMongo(file.name="minimum-wage-tweets.json",
    ns="tweets.minimum_wage")
# NOTE: this function adds two additional fields to the data required
# for the rest of the analysis: 1) timestamp (datetime in ISO format)
# and 2) random_number (a random float, from 0 to 1)

# See https://github.com/SMAPPNYU/smappR for more examples of how
# to work with tweets in MongoDB format. Eg:
mongo <- mongo.create(db="tweets")
count.tweets("tweets.minimum_wage")


