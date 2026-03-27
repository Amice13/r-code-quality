library(data.table)
library(cooccur)
library(dplyr)
library(tibble)



# Read File
df <- read.csv(file = 'DYSampleData.csv', stringsAsFactors = FALSE)
# The DYSampleData file contains 1000 random records. The file used for analysis contained around 33,000 records.
# The sample file is for demonstration purposes only and has no analytic value.


#Filter out Mentioned Values
df <- df %>% filter(PresentMentioned != 'Mentioned')
#Because characters can be either present or mentioned at a location, all of those who are mentioned are removed.

#Set Family to "None" for NA Values
df$Family[which(df$Family=="")] <- "None"

#The Bundren and De Spain are created through character name.
df[grepl("Bundren", df$CharacterName), "Family"] <-"Bundren"
df[grepl("De Spain", df$CharacterName, ignore.case = TRUE), "Family"] <-"De Spain"

#There are very few Asian characters in Faulkner. As they do not register in co-occurrence analysis, they
#are removed.
df <- df %>% filter(Race != 'Asian')


#Delete events with any of the groups. 
#As it is not possible to quantify groups, any events including groups have been removed. Groups appear in roughly 
#20% of all events are not insubstantial. At the moment, there is no way to account for them yet.

df.deletegroups <- df %>% 
  filter(Gender == 'Multi Gender Group'| 
           Class == 'MultiClass Group'| 
           Race == 'Multiracial Group'|
          IndividualGroup!='Individual')%>%
  select("EventKey")
df <- df %>% anti_join(df.deletegroups, by = 'EventKey')

#Consolidate Yeoman and Lower Class
#There are too few characters in the Yeoman class to register for analysis. These have been moved to "Lower Class"

df$Class[df$Class == 'Yeoman'] <- 'Lower Class'

#Consolidate all Native Americans into one class.
#The divisions between Native Americans are too granular to register in the analysis.
#Therefore these have been consolidated.

df$Class[df$Class == 'Indian Chief'] <- 'Tribal Member'
df$Class[df$Class == 'Indian Tribal Leader'] <- 'Tribal Member'
df$Class[df$Class == 'Indian Tribal Member'] <- 'Tribal Member'
df$Class[df$Class=='Enslaved Black'] <- 'Enslaved'
df$Class[df$Class=='Free Black'] <- 'Free'

df$Race <- as.factor(df$Race)
df$Class <- as.factor(df$Class)
df$Gender <-as.factor(df$Gender) 
df$EventKey <- as.factor(df$EventKey)


#Cast Family and Event abundance matrix
df.family <- df
df.family <- setDT(df.family)
#Due to the deprecation of the reshape2 dependency in the data.table library, the data frame has to be
#manually coerced into a data.table object.
df.family.abundance <- dcast(df.family, Family~EventKey, length)  
df.family.abundance <- df.family.abundance %>% remove_rownames %>% column_to_rownames(var="Family")


#Generate Race, Class, Gender co-occurrence matrix
#Turn characters into species by concatenating them and cast a data frame of race, class, and gender co-occurrence
#by events.

df.rcg <- df
df.rcg$RaceClassGender <- NA
df.rcg$RaceClassGender <- paste(df.rcg$Race, df.rcg$Class, df.rcg$Gender, sep = " ")
df.rcg.cnt <- as.data.frame(with(df.rcg, table(RaceClassGender, EventKey)) > 0L) +0L


#All Race, Class, Gender tabulation in df.rcg.cnt
#===================================================================================


#Generate Family count of all co-occurrences None exclusive. 

row.names.remove<- c("None")
df.family.abundance <- df.family.abundance[!(row.names(df.family.abundance) %in% row.names.remove),]

#Create Familial and non-Familial data frames. Familial includes all inter-and intra-familial relationships
#non-Familial includes all extra-familial and general relationships. 

#Generate a co-occurrence matrix of race, class, gender, where family abundance is greater than 1.
df.familial <- as.data.frame (df.rcg.cnt[1:nrow(df.rcg.cnt),colSums(df.family.abundance) > 1])


#Generate a co-occurrence matrix of race, class, gender, where family abdundance is equal to or less than 1.
df.nonfamilial <- as.data.frame (df.rcg.cnt[1:nrow(df.rcg.cnt),colSums(df.family.abundance) <= 1])



#Generate Co-occurrence object using cooccur package. This checks to see what elements within in the data set
#appear at the same event and then calculates the probability of that happening given the other data.
#These are computationally expensive functions and take time to process.


df.rcg.family.present.cooccur <- cooccur(mat = df.familial,
                                        type="spp_site",
                                        thresh=TRUE,
                                        spp_names = TRUE,
                                        true_rand_classifier = 0.1, prob = "comb",
                                        site_mask = NULL, only_effects = FALSE,
                                        eff_standard = FALSE, eff_matrix = FALSE
)

#Generate summary data for analysis
plot(df.rcg.family.present.cooccur)
summary(df.rcg.family.present.cooccur)

df.rcg.family.absent.cooccur <- cooccur(mat = df.nonfamilial,
                                         type="spp_site",
                                         thresh=TRUE,
                                         spp_names = TRUE,
                                         true_rand_classifier = 0.1, prob = "comb",
                                         site_mask = NULL, only_effects = FALSE,
                                         eff_standard = FALSE, eff_matrix = FALSE
)

plot(df.rcg.family.absent.cooccur)
summary(df.rcg.family.absent.cooccur)



#Represent the results as a probability table. 
#Sort the tables and then add a column to each with a key to match values called "spkey"
df.rcg.present.probtable <- prob.table(df.rcg.family.present.cooccur)
df.rcg.absent.probtable <- prob.table(df.rcg.family.absent.cooccur)
df.rcg.present.probtable$spkey <- NA
df.rcg.present.probtable$spkey <- paste(df.rcg.present.probtable$sp1_name, df.rcg.present.probtable$sp2_name, sep = ".")
df.rcg.absent.probtable$spkey <- NA
df.rcg.absent.probtable$spkey <- paste(df.rcg.absent.probtable$sp1_name, df.rcg.absent.probtable$sp2_name, sep = ".")


#This is a borrowed function that merges two tables, adds a column for the old values, one for the new, 
#and one for the delta. It passes in the old table, the new table, the record key and then the values for which
#new columns should be created.

df.changes <- function(df.old, df.new, 
                       KEYS = c("id"),
                       VAL = NULL,
                       retain.columns = NULL) {
  # input checks 
  stopifnot(KEYS %in% names(df.old),
            KEYS %in% names(df.new),
            VAL %in% names(df.old),
            VAL %in% names(df.new),
            retain.columns %in% names(df.new),
            retain.columns %in% names(df.old))
  
  # add columns to help us track new/old provenance
  N <- transform(df.new, is = TRUE)
  O <- transform(df.old, is = TRUE)
  
  # merge
  M <- merge(N, O, by = KEYS, all = TRUE, suffixes = c(".new",".old"))
  M$is.new <- !is.na(M$is.new) # replace NA with FALSE
  M$is.old <- !is.na(M$is.old) # replace NA with FALSE
  
  #output
  O <- M[KEYS]
  
  # add rows.changed
  O$row.changed <- with(M, ifelse(is.old & is.new, "10.Retained",
                                  ifelse(is.old,          "05. Lost",
                                         "00. New")))
  # add data from new
  original.vars <- setdiff(names(df.new), KEYS)
  for (var in original.vars)
    O[[var]] <- M[[paste0(var, ".new")]]
  
  # modify data for retain.columns
  for (var in retain.columns)
    O[[var]] <- ifelse(M$is.new, M[[paste0(var, ".new")]],
                       M[[paste0(var, ".old")]])
  
  # add comparisons
  for (var in VAL) {
    old.var <- paste0(var, ".old")
    new.var <- paste0(var, ".new")
    del.var <- paste0(var, ".delta")
    O[[del.var]] <- M[[new.var]] - M[[old.var]]
    O[[old.var]] <- M[[old.var]]
    O[[new.var]] <- M[[new.var]]
  }
  
  # reorder rows
  O[order(O$row.changed), ]
}

#Read all changes between the two data tables into a data.frame using the changes function.

df.allchanges <- df.changes(df.rcg.absent.probtable, 
                            df.rcg.present.probtable, 
                            KEYS = "spkey", 
                            VAL = c("obs_cooccur","prob_cooccur", "exp_cooccur", "p_lt", "p_gt")
                            )

#Generate version stamp
lgd <- format(Sys.time(), "%Y%m%d%H%M")
current.version <- paste("Effect_of_Family_on_Co-Occurance ver_", lgd, ".csv", sep="" )

#Generate final output
write.csv(df.allchanges, current.version)
