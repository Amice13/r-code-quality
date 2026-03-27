
#setwd("Replication")

print("Welcome Replicator. We start our script now.")


load("./data/tweets.rdata")


rm(pol)
gc()
## Turn to corpus  

library(quanteda)
fc<-corpus(as.character(testtweets$text),docvars = testtweets)

source("tools/toolbox.R")


#names(docvars(fc))

ft<-tokens(fc,remove_punct=T,remove_numbers = T,remove_url = T)

namelist2<-unique(unlist(strsplit(unique(as.character(docvars(fc,"Name")))," ")))
namelist3<-unique(unique(as.character(docvars(fc,"party"))))


ft<-tokens_tolower(ft)

# Removing Stopwords and Politician Names
ft<-tokens_select(ft,pattern=stopwords(),selection='remove')
ft<-tokens_select(ft,pattern=namelist2,selection='remove')
ft<-tokens_select(ft,pattern=namelist3,selection='remove')

gc()

# Removing Handles
#ft<-tokens_select(ft,pattern='@*',selection='remove')
#ft<-tokens_select(ft,pattern='#*',selection='remove')
ft<-tokens_select(ft,pattern='+',selection='remove')
ft<-tokens_select(ft,pattern='=',selection='remove')


dfc<-dfm(ft)


library(lubridate)
# Removing abbreviations and authortags -> some politicians use their initials to
# indicate authorship and this heavily clusters and biases


#dfc<-dfm_select(dfc,featnames(dfc)[nchar(featnames(dfc))>3])
tim<-as.POSIXct(docvars(dfc,"created_at"))
docvars(dfc,"time")<-tim


prim<-as.POSIXct(docvars(dfc,"primary_date"),format = "%d.%m.%Y")
docvars(dfc,"primary_date")<-prim
docvars(dfc,"placebo_date")<-sample(tim,ndoc(dfc))
plac<-as.POSIXct(docvars(dfc,"placebo_date"),format = "%d.%m.%Y")
docvars(dfc,"ttp")<-round(difftime(tim,prim,unit="weeks"),0)
docvars(dfc,"ttp_plac")<-round(difftime(tim,plac,unit="weeks"),0)

week<-round_date(docvars(dfc,"time"),"week")
docvars(dfc,"week")<-week

month<-round_date(docvars(dfc,"time"),"month")
docvars(dfc,"month")<-month


docvars(dfc,"partymonth")<-paste0(docvars(dfc,"party"),"-",month)
docvars(dfc,"partyweek")<-paste0(docvars(dfc,"party"),"-",week)
docvars(dfc,"partyweekwin")<-paste0(docvars(dfc,"party"),"-",docvars(dfc,"week"),"-",docvars(dfc,"winner"))
docvars(dfc,"partytimewin")<-paste0(docvars(dfc,"party"),"-",docvars(dfc,"ttp"),"-",docvars(dfc,"winner"))
docvars(dfc,"partyplacwin")<-paste0(docvars(dfc,"party"),"-",docvars(dfc,"ttp_plac"),"-",docvars(dfc,"winner"))




save(dfc,file="./data/us_dfmn.rdata")




## Validation


load("./data/validation_tweets.rdata")


library(quanteda)
fc<-corpus(as.character(tval$text),docvars = tval)


names(docvars(fc))

ft<-tokens(fc,remove_punct=T,remove_numbers = T,remove_url = T)

namelist2<-unique(unlist(strsplit(unique(as.character(docvars(fc,"bioname")))," ")))
#namelist3<-unique(unique(as.character(docvars(fc,"party"))))


ft<-tokens_tolower(ft)

# Removing Stopwords and Politician Names
ft<-tokens_select(ft,pattern=stopwords(),selection='remove')
ft<-tokens_select(ft,pattern=namelist2,selection='remove')
ft<-tokens_select(ft,pattern=namelist3,selection='remove')

gc()

# Removing Handles
#ft<-tokens_select(ft,pattern='@*',selection='remove')
#ft<-tokens_select(ft,pattern='#*',selection='remove')
ft<-tokens_select(ft,pattern='+',selection='remove')
ft<-tokens_select(ft,pattern='=',selection='remove')



dfc<-dfm(ft)
dfc2<-dfc
save(dfc2,file="data/validation_dfm.rdata")

