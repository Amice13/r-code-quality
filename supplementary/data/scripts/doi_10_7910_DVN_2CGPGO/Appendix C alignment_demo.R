#######
#######
####### Replication Data for: The Differential Impact of the Hong Kong National Security Law 
####### on Political Sensitivity Bias in Opinion Polls.
####### This file produces the figures seen in Appendix C.
####### Last Updated: May. 2023
#######
#######

#setup
rm(list = ls()) # clear workspace

need <- c("foreign",  "tidyverse",  "csvy",
          "devtools", "dplyr", "tidyr", "lubridate", "ggiplot") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages


rm(list = ls()) # clear workspace
setwd(gsub("/Taiwan_independence","",dirname(rstudioapi::getSourceEditorContext()$path)))
list.files()

load("Taiwan_independence/59correction.Rdata")
load(paste0("cleaned data/","appraisal_civilization",".Rdata"))
a <- subset(a, select=c(Date, proBeijing, sensitivity))
a$name <- "appraisal_civilization"
a$treated <- 0
appraisal_civilization <- a
rm(a)

indep_Taiwan <- subset(indep_Taiwan, select=-period)
merge_demo <- rbind(indep_Taiwan[1:7,], appraisal_civilization[1:9,])

ggplot(data=merge_demo)+geom_line(aes(x=Date, y=proBeijing, color=name))+
  geom_vline(data=merge_demo[1:7,], aes(xintercept=Date), linetype="dashed", color="black")+
  xlab("Date")+ylab("Unstandardised support for Beijing")+ggtitle("Unaligned poll data demo")

load("Taiwan_independence/59correction.Rdata")
merge <- rbind(merge[1:7,],merge[73:79,])
ggplot(data=merge)+geom_line(aes(x=Date, y=proBeijing, color=name))+
  geom_vline(data=merge_demo[1:7,], aes(xintercept=Date), linetype="dashed", color="black")+
  xlab("Date")+ylab("Unstandardised support for Beijing")+ggtitle("Unaligned poll data demo (trimmed)")


merge$date <- merge$Date[merge$period==merge$period & merge$name=="indep_Taiwan"]
ggplot(data=merge)+geom_line(aes(x=date, y=proBeijing, color=name))+
  geom_vline(data=merge_demo[1:7,], aes(xintercept=Date), linetype="dashed", color="black")+
  xlab("Date")+ylab("Unstandardised support for Beijing")+ggtitle("Aligned poll data demo")
