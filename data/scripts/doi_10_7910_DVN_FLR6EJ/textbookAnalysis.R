###############
### Textbook ##
### Analysis ##
###############

rm(list=ls())
 
# set working directory to "Replication" folder

# dependencies
library(textreadr); library(tidytext); library(tidyr); library(dplyr)
library(stringr); library(topicmodels); library(ggplot2); library(udpipe)
library(tidyverse) ; library(httr) ; library(jsonlite); library(widyr)
require(quanteda); require(seededlda); require(lubridate)

# read in data
load("Replication/holoLegacyReplication2.RData")

###############
### N-GRAMS ###
###############

booksLemma <- as_tibble(booksLemma)

############
# FIGURE 5 #
############

mentions <- NULL
for(i in unique(booksLemma$book)){
  mentions <- c(mentions, str_count(booksLemma$nonstopText[booksLemma$book==i],"שואה"))
}

holocaustPerBook <- NULL
for(i in unique(booksLemma$subject)){
  holocaustPerBook <- c(holocaustPerBook, 
                        sum(str_count(booksLemma$nonstopText[booksLemma$subject==i]," שואה ")) / 
                          length(str_count(booksLemma$nonstopText[booksLemma$subject==i]," שואה ")))
}

positions = c("Bible","Civics","Geography",
              "Hebrew",
              "History",
              "Homeland & Citizenship",
              "Jewish Israeli Culture",
              "Jewish Thought","Life Skills",
              "Music","Oral Torah")
holocaustBySubject <- as.data.frame(cbind(subject=c("Bible","Civics","Geography",
                                                    "Hebrew",
                                                    "History",
                                                    "Homeland & Citizenship",
                                                    "Jewish Israeli Culture",
                                                    "Jewish Thought","Life Skills",
                                                    "Music","Oral Torah"), 
                                          as.numeric(holocaustPerBook)))

ggplot(data=holocaustBySubject, aes(x=positions, y=holocaustPerBook)) +
  geom_bar(stat="identity", color="black", fill="#bae4b3") +
  geom_text(aes(label=round(holocaustPerBook,2)), hjust=-0.5) +
  coord_flip() +
  labs(x="Textbook Subject",y="Average Mentions of Holocaust per Textbook") +
  ylim(0,40) +
  theme_classic() 

booksLemma$grade <- ifelse(str_detect(booksLemma$book, "grade10"),"Grade 10-12",
                           ifelse(str_detect(booksLemma$book, "grade13"),"Grade 10-12",
                                  ifelse(str_detect(booksLemma$book, "grade3"),"Grade 1-5",
                                         ifelse(str_detect(booksLemma$book, "3th"), "Grade 1-5",
                                                ifelse(str_detect(booksLemma$book, "grade4"),"Grade 1-5",
                                                       ifelse(str_detect(booksLemma$book, "grade5"),"Grade 1-5",
                                                              ifelse(str_detect(booksLemma$book, "grade6"),"Grade 6-9",
                                                                     ifelse(str_detect(booksLemma$book, "grade7"),"Grade 6-9",
                                                                            ifelse(str_detect(booksLemma$book, "grade8"),"Grade 6-9",
                                                                                   ifelse(str_detect(booksLemma$book, "grade9"),"Grade 6-9",
                                                                                          ifelse(str_detect(booksLemma$book, "grade1"),"Grade 1-5",
                                                                                                 ifelse(str_detect(booksLemma$book, "grade2"),"Grade 1-5",NA))))))))))))

holocaustPerBookGrade <- NULL
book <- booksLemma[!is.na(booksLemma$grade),]
for(i in unique(book$grade)){
  holocaustPerBookGrade <- c(holocaustPerBookGrade,sum(str_count(book$text[book$grade==i],"שואה")) / 
                               length(str_count(book$text[book$grade==i],"שואה")))
}

holocaustByGrade <- as.data.frame(cbind(grade=unique(book$grade), 
                                        as.numeric(holocaustPerBookGrade)))
positions = c("Grade 1-5","Grade 6-9","Grade 10-12")
ggplot(data=holocaustByGrade, aes(x=grade, y=holocaustPerBookGrade)) +
  geom_bar(stat="identity", color="black", fill="#bae4b3") +
  labs(x="Textbook Grade Level",y="Average Mentions of Holocaust per Textbook") +
  scale_x_discrete(limits = positions) +
  theme_classic() 

################
# DESCRIPTIVES #
################
# (section 6.1)

# Holocaust mentions per book (Civics)
holocaustBySubject$V2[holocaustBySubject=="Civics"]

# Holocaust mentions per book (Hebrew)
holocaustBySubject$V2[holocaustBySubject=="Hebrew"]

# Holocaust mentions per book in elementary school
holocaustByGrade$V2[holocaustByGrade=="Grade 1-5"]

# Percent elementary books mentioning Holocaust
p20 <- cbind(booksLemma,mentions)
p20 <- p20[p20$grade=="Grade 1-5" & !is.na(p20$grade),]
nrow(p20[p20$mentions>0,])/20

# Total mentions of Holocaust
mentionHolocaust <- NULL
for(i in unique(booksLemma$book)){
  mentionHolocaust <- c(mentionHolocaust, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                                    c("שואה")))
}
sum(mentionHolocaust)

# Total mentions of Nazis
mentionNazi <- NULL
for(i in unique(booksLemma$book)){
  mentionNazi <- c(mentionNazi, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                          c("נאצי","נאצים","נאצית","נאציזם")))
}
sum(mentionNazi)

# Total mentions of terrorism
mentionTerrorist <- NULL
for(i in unique(booksLemma$book)){
  mentionTerrorist <- c(mentionTerrorist, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                                    c("טרוריסט"	,"טרוריסטית"	,"טרוריסטים"	,"טרוריסטיות"	,"טרור")))
}
sum(mentionTerrorist)

# Total mentions of terrorism
mentionPalestinian <- NULL
for(i in unique(booksLemma$book)){
  mentionPalestinian <- c(mentionPalestinian, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                                        c("פלסטיני"	,"פלסטינית"	,"פלסטינים"	,"פלסטיניות"	,"פלסטין"	,"פלסטינה")))
}
sum(mentionPalestinian)

# Total mentions of Arabs
mentionArab <- NULL
for(i in unique(booksLemma$book)){
  mentionArab <- c(mentionArab, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                          c("ערבי"	,"ערביה"	,"ערבים"	,"ערביות"	,"ערב"	,"ערבית")))
}
sum(mentionArab)

###########
# TABLE 2 #
###########

textBigram <- booksLemma %>% 
  unnest_tokens(bigram,nonstopText, token = "ngrams", n=2)

bigramsSeparated <- textBigram %>%
  separate(bigram, c("word1","word2"), sep=" ")

bigramCounts <- bigramsSeparated %>%
  count(word1,word2,sort=T)

holocaustBigram <- bigramCounts %>% 
  dplyr::filter(word1=="שואה" | word2=="שואה")

# this is the raw data for Table 2
# we manually combine words with the same meaning to produce Table 2
holocaustBigram

# this words means "memory"
# (referenced in-text section 6.2)
holocaustBigram[1,]

# Total mentions of modern anti-semitism
antisemitismBigram <- bigramCounts %>% 
  filter(word1=="אנטישמיות"| word2=="אנטישמיות")
  # combine rows 1 and 5 from antisemitismBigram to get 43

##################
# TABLES G.1-G.3 #
##################

# Note that these three tables are descriptive and do not include any analyses.
# All three tables were generated manually.

#############
# TABLE G.4 #
#############

mentionZionist <- NULL
for(i in unique(booksLemma$book)){
  mentionZionist <- c(mentionZionist, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                                c("ציונית"	,"ציונים"	,"ציוניות"	,"ציונות"	,"ציון","אנטי-ציוני")))
}
mentionJewish <- NULL
for(i in unique(booksLemma$book)){
  mentionJewish <- c(mentionJewish, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                              c("יהודי"	,"יהודיה"	,"יהודים"	,"יהודיות"	,"יהדות"	,"יהוד"	,"יהודה")))
}
mentionPalestinian <- NULL
for(i in unique(booksLemma$book)){
  mentionPalestinian <- c(mentionPalestinian, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                                        c("פלסטיני"	,"פלסטינית"	,"פלסטינים"	,"פלסטיניות"	,"פלסטין"	,"פלסטינה")))
}
mentionDemocratic <- NULL
for(i in unique(booksLemma$book)){
  mentionDemocratic <- c(mentionDemocratic, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                                      c("דמוקרטי"	,"דמוקרטית"	,"דמוקרטים"	,"דמוקרטיות"	,"דמוקרטיה")))
}
mentionArab <- NULL
for(i in unique(booksLemma$book)){
  mentionArab <- c(mentionArab, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                          c("ערבי"	,"ערביה"	,"ערבים"	,"ערביות"	,"ערב"	,"ערבית")))
}
mentionTerrorist <- NULL
for(i in unique(booksLemma$book)){
  mentionTerrorist <- c(mentionTerrorist, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                                    c("טרוריסט"	,"טרוריסטית"	,"טרוריסטים"	,"טרוריסטיות"	,"טרור")))
}
mentionIsraeli <- NULL
for(i in unique(booksLemma$book)){
  mentionIsraeli <- c(mentionIsraeli, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                                c("ישראלי"	,"ישראלית"	,"ישראלים"	,"ישראליות"	,"ישראל")))
}
mentionState <- NULL
for(i in unique(booksLemma$book)){
  mentionState <- c(mentionState, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                            c("מדינה"	,"מדינות")))
}
mentionHomeland <- NULL
for(i in unique(booksLemma$book)){
  mentionHomeland <- c(mentionHomeland, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                                  c("מולדת")))
}
mentionCitizen <- NULL
for(i in unique(booksLemma$book)){
  mentionCitizen<- c(mentionCitizen, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                               c("אזרח"	,"אזרחית"	,"אזרחים"	,"אזרחיות"	,"מאוזרח"	,"אזרחות")))
}
mentionIdentity <- NULL
for(i in unique(booksLemma$book)){
  mentionIdentity <- c(mentionIdentity, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                                  c("זהות"	,"זהויות"	,"להזדהות"	,"מזדהה")))
}
mentionLand <- NULL
for(i in unique(booksLemma$book)){
  mentionLand <- c(mentionLand, str_count(booksLemma$nonstopText[booksLemma$book==i],
                                          c("ארץ")))
}
group <- c("Nazis","Holocaust","Zionist","Jewish","Palestinian","Democratic",
           "Arab","Terrorist","Israeli","State","Homeland","Citizen","Identity",
           "Land")
count <- c(sum(mentionNazi),sum(mentionHolocaust),sum(mentionZionist),
           sum(mentionJewish),sum(mentionPalestinian),sum(mentionDemocratic),
           sum(mentionArab),sum(mentionTerrorist),sum(mentionIsraeli),
           sum(mentionState),sum(mentionHomeland),sum(mentionCitizen),
           sum(mentionIdentity),sum(mentionLand))
frequencyList <- as.data.frame(cbind(group,count))
frequencyList

