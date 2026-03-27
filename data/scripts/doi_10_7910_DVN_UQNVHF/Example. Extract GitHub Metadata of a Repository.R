

#####################################
### First Step: Clone Repository  ###
#####################################

setwd("YOUR.WORKING.DIRECTORY")
system('git clone --no-checkout https://github.com/hyperledger/fabric.git')

# Note: You can access the url address directly from GitHub

setwd("YOUR.WORKING.DIRECTORY/REPONAME")

# If you want to update to the lastest info instead: 
system('git reset --hard')
system('git pull')


####################################
### Second Step: Parse Repo Info ###
####################################
library(stringi)
library(readr)
library(data.table)
library(dplyr)

setwd("YOUR.WORKING.DIRECTORY/REPONAME")

## Get Summary Info
system('git log --pretty=format:"%h§%an§%aE§%cd§\"%s\"§%ad§%aN§%aE§%cd§\"%N\"" > log.csv')
log <- read_delim("log.csv",col_names=FALSE,delim = '§')
log=log[,1:8]
file.remove("log.csv")
colnames(log)=c('Commit','AuthorName','AuthorEmail','AuthorDate','Title','CommitDate','CommiterName','CommiterEmail')

## Stats Info
system('git log --pretty=format:"%nCommitDelimiter" --shortstat > code_diff.txt') 
stats <- paste(read_lines("code_diff.txt"),collapse = '')
stats=stri_split_fixed(stats,'CommitDelimiter')[[1]]
stats=stringi::stri_trans_general(stats, "latin-ascii")
stats=stats[-1]
Merges=stats=='' # Avoid duplicated information
Merges[Merges][nchar(log$Commit[Merges])>15]=FALSE # Avoid duplicated information
system(paste('git show --pretty=format:"%nCommitDelimiter" --shortstat ',paste(log$Commit[Merges],collapse = ' '),' > code_diff.txt',collapse = ' '))
stats0 <- paste(read_lines("code_diff.txt"),collapse = '')
stats0=stri_split_fixed(stats0,'CommitDelimiter')[[1]]
stats0=stringi::stri_trans_general(stats0, "latin-ascii")
stats0=stats0[-1]
stats[Merges]=stats0

FilesChanged=as.numeric(unlist(sapply(stats,function(r) {
  temp=stri_replace_all_fixed(r,'file changed','fileschanged')
  temp=stri_replace_all_fixed(temp,'files changed','fileschanged')
  temp=stri_split_fixed(temp,' ')[[1]]
  I=I(1:length(temp))[temp=='fileschanged,']
  temp=temp[I-1]
  return(ifelse(length(temp)==0,0,temp))
})))
Insertions=as.numeric(unlist(sapply(stats,function(r) {
  temp=stri_replace_all_fixed(r,'file changed','fileschanged')
  temp=stri_replace_all_fixed(temp,'files changed','fileschanged')
  temp=stri_split_fixed(temp,' ')[[1]]
  I=I(1:length(temp))[temp=='fileschanged,']
  temp=temp[I+1]
  return(ifelse(length(temp)==0,0,temp))
})))
Deletions=as.numeric(unlist(sapply(stats,function(r) {
  temp=stri_replace_all_fixed(r,'file changed','fileschanged')
  temp=stri_replace_all_fixed(temp,'files changed','fileschanged')
  temp=stri_split_fixed(temp,' ')[[1]]
  I=I(1:length(temp))[temp=='fileschanged,']
  temp=temp[I+3]
  return(ifelse(length(temp)==0,0,temp))
})))
file.remove("code_diff.txt")


# Data Transformation & Additions
Data=data.frame(log,FilesChanged,Insertions,Deletions)
Data$Year=as.numeric(sapply(Data$AuthorDate,function(r) stri_split_fixed(r,' ')[[1]][5]))
Data$IsMerge=Merges

# Save
save(Data,'RepoMetaData.RData')

