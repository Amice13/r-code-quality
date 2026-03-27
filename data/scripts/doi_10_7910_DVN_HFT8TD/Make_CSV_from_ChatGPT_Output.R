#Check for and Load Necessary Packages (from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them)
list.of.packages <- c("stringr", "rjson","svDialogs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


#Get Input from  User and Set Defaults 
gptdata.dir = dlgInput("Enter the full file path to the folder containing ChatGPT converation data:",default = getwd())$res
thefilename = dlgInput("Enter a name of the conversation data you are processing (no spaces!)",default = "mygptdf")$res

#Function to Create Dataset
convert.gpt.json.to.csv <- function(thefilepath = gptdata.dir,filename = thefilename){
  #Set Working Directory to Folder of ChatGPT Output
  setwd(thefilepath)
  
  #Import the Conversations .json file
  raw = fromJSON(file = "conversations.json")
  
  #Extract the element CSV Content
  raw = unlist(raw)
  raw = raw[which(grepl("\n\n```",raw)==TRUE )]
  
  #Remove all the text not contained in the CSV output
  hold = word(raw,2,sep = "\n\n```")
  hold2 = sub(".*?\n",'',hold)
  hold3 = word(hold2,1,sep = "\n```\n\n")
  
  #Reformat the CSV text into a Dataframe
  if(length(hold3)>1){
    df = read.table(text = hold3[1],sep = ",",header = TRUE,stringsAsFactors = FALSE)
    for(i in 2:length(hold3)){
      df = rbind(df,read.table(text = hold3[i],sep = ",",header = TRUE,stringsAsFactors = FALSE))
    }
  }else{
    df = read.table(text = hold3[1],sep = ",",header = TRUE,stringsAsFactors = FALSE)
  }
  
  #Save Dataframe as CSV File
  write.csv(df,paste(filename,"_chatgpt_output.csv",sep = ""))
}


convert.gpt.json.to.csv()



dlg_message(paste("Your CSV file, ", thefilename, "_chatgpt_output.csv, has been saved here:",gptdata.dir,sep = ""),type = "ok")









