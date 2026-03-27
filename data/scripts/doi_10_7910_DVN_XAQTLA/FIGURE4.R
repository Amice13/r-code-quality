# Arthur Spirling
# 10/15/2024

rm(list=ls())
set.seed(1649)

# PURPOSE: give a graphical overview of the data over time.
# It also provides a summary proportion of the number of pamphlets first authored
# by the "Levellers" (as defined by us)

##############################
# Load packages, make corpus #
##############################

require(quanteda)
require(readtext)
#grab the tar files and unpack
untar("leveller_texts.tar")
untar("useful_objects.tar")

# get the doc variables for the corpus
overview <-  read.csv("useful_objects/overview_files_data.csv")

# load the corpus
# get the Leveller pamphlet collection
setwd("leveller_texts")
rtL <- readtext("*.txt")


# set the variables up
m <-  match(rtL$doc_id, overview$DocCode)
variables <- overview[m,]

rtL_corp <- corpus(rtL)
docvars(rtL_corp) <- variables

token_vector <- ntoken(rtL_corp)


####################
#  plot over time  #
####################


# get the dates of these 
rc <-  overview
source("../useful_objects/make_dates.R")
rc_dates <- make_dates(rc)
mm <- match(docvars(rtL_corp)$DocCode,rc$DocCode)
docvars(rtL_corp)$date <- rc_dates[mm]



dates_length <- data.frame(author= docvars(rtL_corp)$FirstAuthor, 
                           date= docvars(rtL_corp)$date, 
                           len = token_vector, 
                           mainL = docvars(rtL_corp)$leveller_main , 
                           eL = docvars(rtL_corp)$leveller_expansive)

dates_len <- dates_length[order(dates_length$date),]

# log the lengths to make comparison a bit easier
dates_len$len <- log(dates_len$len, b=10 )


# now plot the summary

x11()
plot(dates_len$date, dates_len$len, type="n", pch=16, ylab="log(length)",xlab="Year" )  
abline(v=as.Date("1645-08-30"), lty=2, lwd=2)
abline(v=as.Date("1649-11-16"), lty=2, lwd=2)


levsm <- dates_len[dates_len$mainL==1,] # main levellers
levse <- dates_len[dates_len$mainL==0&dates_len$eL==1,] #expanded definition
nonlev <- dates_len[dates_len$eL==0&dates_len$mainL==0,] # non-levellers

points(levsm$date, levsm$len, col="black", bg=rgb(red=1, blue=0, green=0, alpha=0.5), pch=21, cex=1.5)
points(levse$date, levse$len, col="black", bg="pink", pch=22, cex=1.5)
points(nonlev$date, nonlev$len, col="black", bg=rgb(red=0, blue=1, green=0, alpha=0.1), pch=25, cex=1.5)
#grid(NULL, NULL)

legend(x = "bottomright",          # Position
       legend = c("Leveller (main)", "Leveller (other)","non-Leveller"),  # Legend texts
       pt.bg = c(rgb(red=1, blue=0, green=0, alpha=0.5),"pink",rgb(red=0, blue=1, green=0, alpha=0.1)),
       col =  c("black","black","black"), 
       pch = c(21,22,25) ,
       pt.cex =c(1.5,1.5,1.5),
       cex=c(1.2,1.2,1.2), bty="n",bg='lightgray' )


######################
# summary table      #
# of first authors   #
######################


author_prop <- sort(table(docvars(rtL_corp)$FirstAuthor)/sum(table(docvars(rtL_corp)$FirstAuthor)), dec=TRUE)
# Lilburne (0.21), Overton (0.08), Walwyn (0.12) and Thomas Prince (0.004)
# John Wildman (0.0114) and Edward Sexby (0.015)

# what proportion were (first author) written by "our" Levellers?
leveller_authors <- c("Lilburne", "Overton", "Walwyn","Prince","Wildman", "Sexby")
matched <- unique (grep(paste(leveller_authors,collapse="|"), 
                      names(author_prop), value=TRUE))

# about 40% are first authored by those defined by us as Levellers:
cat("\n Leveller first authors proportion of corpus:\n")
print(sum(author_prop[matched]))

