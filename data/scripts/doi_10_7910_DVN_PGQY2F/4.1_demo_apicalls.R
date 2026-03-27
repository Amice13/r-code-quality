#############################################################################################
# Replication Data for: Proksch, Wratil, Wäckerle. (2018). Testing the Validity of Automatic
# Speech Recognition for Political Text Analysis. Political Analysis, forthcoming.
#############################################################################################

#########################
# This script provides a demonstration of how we used the API calls.
# Please read up on the googleLanguageR package here: https://code.markedmondson.me/googleLanguageR/articles/speech.html
# You will need own credentials for making API calls and an account for storing the video files that should be transcribed.

library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
library(googleLanguageR)
gl_auth("XXXXXXXXX") #Fill in your Google API authentification here

# Example for downloading one debate in the MFF debates.
async_3107a <- gl_speech("XXX",#link to a FLAC file stored in the google cloud storage
                      encoding = "FLAC", asynch = TRUE,languageCode = "en-GB",sampleRateHertz = 22050)
async_3107a_final<-gl_speech_op(async_3107a)
text_3107a=paste(async_3107a_final$transcript$transcript,collapse = " ")
write(text_3107a,paste0("MFF/Textfiles/API/","3107a_api.txt"))