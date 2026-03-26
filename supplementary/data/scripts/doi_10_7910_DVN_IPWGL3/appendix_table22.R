##--------------------------------------------------- ###
## Make table of number of coded reports and the identifier (S/XXXX/XXX) 
## of each report by PKO Name and Country
##--------------------------------------------------- ###

install.packages("readxl","dplyr","xtable")

library(readxl)
library(dplyr)
library(xtable)

setwd(".../Replication") #Set working directory as "Replication" folder

PACT <- read_excel("pkoreports_appendix_table22.xlsx")

PACT <- PACT[PACT$OriginalSummary=="Original",c(1:10)]

# How many reports
length(unique(PACT$ReportNumber))

# How many reports per year
PACT$year = NA
for(i in 1:nrow(PACT)){
  PACT$year[i] <- substr(  PACT$DateOfReport[i]
                      , as.numeric(stringr::str_length(PACT$DateOfReport[i]))-3
                      , as.numeric(stringr::str_length(PACT$DateOfReport[i])) )   
}
PACT$year <- as.numeric(as.character(PACT$year))


PACT_summary <- PACT %>% group_by(NamePKO, year) %>%
                          summarize( NumberOfReports = n()) %>%
                          as.data.frame()
PACT_summary
table(PACT_summary$NumberOfReports)
table(PACT_summary$NumberOfReports[PACT_summary$year>=2000])

# How many reports per mission
PACT_summary <- PACT %>% group_by(NamePKO) %>%
                         summarize( NumberOfReports = n()
                                   ,IdentifiersOfReports = toString(ReportNumber)) %>%
                         as.data.frame()

PACT_summary$Country <- ""
PACT_summary$Country[PACT_summary$NamePKO=="MINURCA"] <- "Central African Republic"
PACT_summary$Country[PACT_summary$NamePKO=="MINURCAT"] <- "Central African Republic"
PACT_summary$Country[PACT_summary$NamePKO=="MINUSCA"] <- "Central African Republic"
PACT_summary$Country[PACT_summary$NamePKO=="MINUSMA"] <- "Mali"
PACT_summary$Country[PACT_summary$NamePKO=="MONUA"] <- "Angola"
PACT_summary$Country[PACT_summary$NamePKO=="MONUC"] <- "Democratic Republic of the Congo"
PACT_summary$Country[PACT_summary$NamePKO=="MONUSCO"] <- "Democratic Republic of the Congo"
PACT_summary$Country[PACT_summary$NamePKO=="ONUB"] <- "Burundi"
PACT_summary$Country[PACT_summary$NamePKO=="ONUMOZ"] <- "Mozambique"
PACT_summary$Country[PACT_summary$NamePKO=="UNAMID"] <- "Sudan (Darfur)"
PACT_summary$Country[PACT_summary$NamePKO=="UNAMIR"] <- "Rwanda"
PACT_summary$Country[PACT_summary$NamePKO=="UNAMSIL"] <- "Sierra Leone"
PACT_summary$Country[PACT_summary$NamePKO=="UNAVEM I"] <- "Angola"
PACT_summary$Country[PACT_summary$NamePKO=="UNAVEM II"] <- "Angola"
PACT_summary$Country[PACT_summary$NamePKO=="UNAVEM III"] <- "Angola"
PACT_summary$Country[PACT_summary$NamePKO=="UNIFSA"] <- "Angola"
PACT_summary$Country[PACT_summary$NamePKO=="UNMIL"] <- "Liberia"
PACT_summary$Country[PACT_summary$NamePKO=="UNMIS"] <- "Sudan"
PACT_summary$Country[PACT_summary$NamePKO=="UNMISS"] <- "South Sudan"
PACT_summary$Country[PACT_summary$NamePKO=="UNOCI"] <- "Ivory Coast"
PACT_summary$Country[PACT_summary$NamePKO=="UNOMSIL"] <- "Sierra Leone"
PACT_summary$Country[PACT_summary$NamePKO=="UNSOM I"] <- "Somalia"
PACT_summary$Country[PACT_summary$NamePKO=="UNSOM II"] <- "Somalia"
PACT_summary$Country[PACT_summary$NamePKO=="UNTAG"] <- "Namibia"

# How many countries and missions?
length(unique(PACT_summary$Country))
length(unique(PACT_summary$NamePKO))

# Make table
PACT_summary <- PACT_summary %>% arrange(Country, NamePKO) %>% 
                                 select(Country, NamePKO, NumberOfReports, IdentifiersOfReports) %>%
                                 as.data.frame()


print(xtable(PACT_summary), include.colnames = FALSE, include.rownames = FALSE)

PACT_summary <- PACT_summary %>% arrange(Country, NamePKO) %>% 
                                  select(Country, NamePKO, NumberOfReports) %>%
                                  as.data.frame()


print(xtable(PACT_summary), include.colnames = FALSE, include.rownames = FALSE)