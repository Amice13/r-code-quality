


# 1 tidy ----------------------------------------------------------------------


tidycorp <- tidy(allphrase_corpus,collapse="\n")

tidycorp$datetimestamp <- dfphrase$date[match(tidycorp$id, dfphrase$doc_id)]


# 2 phrases ----------------------------------------------------------------------


city <- c("sinjar","ramadi","tal","afar","fallujah","bayji","kisik","zawr","dayr","mar'","manbij","qayyarah","hasakah","abdallah","sultan","baghdadi","kobani",
          "kamal","huwayjah","ayn","isa","haditha","habbaniyah","kirkuk","qaim","rawah","hawl","makhmur","hayat","asad","shadaddi","palmyra","rutbah","aleppo","bashir",
          "tuz","abyad","washiyah","bab","wale","anbar","taji","idlib","tanf","tikrit","baghdad","dayz","tamakh","zayr","dawr","samarra","erbil",
          "shaddadi","habbaniya","hawayjah","shadadi","washiya","awr","hami","huwayja","irbil","jibbin","mahkmur","abayad","bah","baqubah","barghooth","bukam","harri","hawijah",
          "huwijah","iblib","kamak","palymyra","qayayyarah","rut","tadmur","tallafar","vnear","walwe",
          "tabqah","hajin","khanukah","abdullah","hamrin","jalawla‚Äô","qara","rawa","tapa","taqbah",
          "wadi","ashai","basheer","alqaim","bek","makmur","mayadin","sulayman","alasad","alhasakah","alrutbah",
          "jazeerah","rabiyah","shaddai","tabqa","taqba","zumar","abukam","alanbar","alba‚Äôaj","alhaskah","alkisik",
          "atshana","pes","pocket","qadisiyyah","qaida","qayyrarah","qurayat","rahwah","rga","sha‚Äôfah","shadaddai",
          "sharqat","sharra","talab","talafar","thar","tirkrit","tubal","wadiashai","zagatoon","zwar",
          "raqqa","huwajah","hwayjah","peshmerga","sha‚Äôfah","soor","tel","vicin","ash","sha’fah","ninewah","nera",
          "khusham","jazeera","jalula","jalawla","haskah","haram","hajjaj","dulab","diyala","dhiban","dashisha","baji","baaj","aynzalah","atashanah","anah","aissa",
          "ba’aj","mosul","raqqah","hayi","tall","mar’")

# Phrases

tidycorp$text <- tidycorp$text %>%
  str_replace_all(my_replacements)

tidycorp$location <- rep(NA,nrow(tidycorp))

tidycorp$location <- ifelse(grepl(paste(city, collapse = "|"), tidycorp$text),str_extract(tidycorp$text, paste(city, collapse = "|")),NA)


# 3 unique ----------------------------------------------------------------------



alltokens_prephrase <- tidycorp %>%
  unnest_tokens(word,text) %>%
  distinct()


# 4 remove ----------------------------------------------------------------------


alltokens <- alltokens_prephrase

alltokens <- alltokens[!alltokens$word %in% unimportantterms, ]


# 5 categories ----------------------------------------------------------------------

alltokens$category <- alltokens$word

alltokens$category[alltokens$word %in% residential_buildings_other_critical_infrastructure] <- "residential_buildings_other_critical_infrastructure"
alltokens$category[alltokens$word %in% vehicles_not_military_oil] <- "vehicles_not_military_oil"
alltokens$category[alltokens$word %in% no_category] <- "no_category"
alltokens$category[alltokens$word %in% terrain] <- "terrain"

alltokens$category[alltokens$word %in% business_finance] <- "business_finance"
alltokens$category[alltokens$word %in% construction_equipment] <- "construction_equipment"
alltokens$category[alltokens$word %in% electrical_infrastructure] <- "electrical_infrastructure"
alltokens$category[alltokens$word %in% media_telecom] <- "media_telecom"
alltokens$category[alltokens$word %in% personnel_equipment_facilities] <- "personnel_equipment_facilities"
alltokens$category[alltokens$word %in% oil_infrastructure] <- "oil_infrastructure"
alltokens$category[alltokens$word %in% transportation_infrastructure] <- "transportation_infrastructure"
alltokens$category[alltokens$word %in% water_infrastructure] <- "water_infrastructure"
alltokens$category[alltokens$word %in% manufacturing_production_industrial_infrastructure] <- "manufacturing_production_industrial_infrastructure"


# 6 unique ----------------------------------------------------------------------

data_stemmed_categorized <- alltokens
write.csv(data_stemmed_categorized,"~/Desktop/20250328 Replication Files & Appendix/data_stemmed_categorized.csv")
# Creates an output with the stemmed and categorized text

alltokens <- alltokens %>%
  distinct(id,category, .keep_all = TRUE)


# 7 count ----------------------------------------------------------------------



alltokens$id <- as.numeric(alltokens$id)

finalcount <- alltokens %>%
  count(category,sort=T)


# 8 mosul ----------------------------------------------------------------------

citycorp <- tidycorp %>%
  filter(location %in% c("mosul"))

citytokens <- alltokens %>%
  filter(location %in% c("mosul"))

citycount <- citytokens %>%
  count(category,sort=T)

# Quarterly

#2018
df18q1 <- citytokens %>%
  filter(datetimestamp <= '2018-03-31' & datetimestamp >= '2018-01-01')
count18q1 <- df18q1 %>%
  count(category,sort=T) %>%
  rename(docs2018q1 = n)
df18q2 <- citytokens %>%
  filter(datetimestamp <= '2018-06-30' & datetimestamp >= '2018-04-01')
count18q2 <- df18q2 %>%
  count(category,sort=T) %>%
  rename(docs2018q2 = n)
df18q3 <- citytokens %>%
  filter(datetimestamp <= '2018-09-30' & datetimestamp >= '2018-07-01')
count18q3 <- df18q3 %>%
  count(category,sort=T) %>%
  rename(docs2018q3 = n)
df18q4 <- citytokens %>%
  filter(datetimestamp <= '2018-12-31' & datetimestamp >= '2018-10-01')
count18q4 <- df18q4 %>%
  count(category,sort=T) %>%
  rename(docs2018q4 = n)
#2017
df17q1 <- citytokens %>%
  filter(datetimestamp <= '2017-03-31' & datetimestamp >= '2017-01-01')
count17q1 <- df17q1 %>%
  count(category,sort=T) %>%
  rename(docs2017q1 = n)
df17q2 <- citytokens %>%
  filter(datetimestamp <= '2017-06-30' & datetimestamp >= '2017-04-01')
count17q2 <- df17q2 %>%
  count(category,sort=T) %>%
  rename(docs2017q2 = n)
df17q3 <- citytokens %>%
  filter(datetimestamp <= '2017-09-30' & datetimestamp >= '2017-07-01')
count17q3 <- df17q3 %>%
  count(category,sort=T) %>%
  rename(docs2017q3 = n)
df17q4 <- citytokens %>%
  filter(datetimestamp <= '2017-12-31' & datetimestamp >= '2017-10-01')
count17q4 <- df17q4 %>%
  count(category,sort=T) %>%
  rename(docs2017q4 = n)
#2016
df16q1 <- citytokens %>%
  filter(datetimestamp <= '2016-03-31' & datetimestamp >= '2016-01-01')
count16q1 <- df16q1 %>%
  count(category,sort=T) %>%
  rename(docs2016q1 = n)
df16q2 <- citytokens %>%
  filter(datetimestamp <= '2016-06-30' & datetimestamp >= '2016-04-01')
count16q2 <- df16q2 %>%
  count(category,sort=T) %>%
  rename(docs2016q2 = n)
df16q3 <- citytokens %>%
  filter(datetimestamp <= '2016-09-30' & datetimestamp >= '2016-07-01')
count16q3 <- df16q3 %>%
  count(category,sort=T) %>%
  rename(docs2016q3 = n)
df16q4 <- citytokens %>%
  filter(datetimestamp <= '2016-12-31' & datetimestamp >= '2016-10-01')
count16q4 <- df16q4 %>%
  count(category,sort=T) %>%
  rename(docs2016q4 = n)
#2015 
df15q1 <- citytokens %>%
  filter(datetimestamp <= '2015-03-31' & datetimestamp >= '2015-01-01')
count15q1 <- df15q1 %>%
  count(category,sort=T) %>%
  rename(docs2015q1 = n)
df15q2 <- citytokens %>%
  filter(datetimestamp <= '2015-06-30' & datetimestamp >= '2015-04-01')
count15q2 <- df15q2 %>%
  count(category,sort=T) %>%
  rename(docs2015q2 = n)
df15q3 <- citytokens %>%
  filter(datetimestamp <= '2015-09-30' & datetimestamp >= '2015-07-01')
count15q3 <- df15q3 %>%
  count(category,sort=T) %>%
  rename(docs2015q3 = n)
df15q4 <- citytokens %>%
  filter(datetimestamp <= '2015-12-31' & datetimestamp >= '2015-10-01')
count15q4 <- df15q4 %>%
  count(category,sort=T) %>%
  rename(docs2015q4 = n)
#2014
df14q4 <- citytokens %>%
  filter(datetimestamp <= '2014-12-31' & datetimestamp >= '2014-10-01')
count14q4 <- df14q4 %>%
  count(category,sort=T) %>%
  rename(docs2014q4 = n)

quarterlycounts <- merge(citycount, count14q4[, c("category", "docs2014q4")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count15q1[, c("category", "docs2015q1")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count15q2[, c("category", "docs2015q2")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count15q3[, c("category", "docs2015q3")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count15q4[, c("category", "docs2015q4")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count16q1[, c("category", "docs2016q1")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count16q2[, c("category", "docs2016q2")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count16q3[, c("category", "docs2016q3")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count16q4[, c("category", "docs2016q4")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count17q1[, c("category", "docs2017q1")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count17q2[, c("category", "docs2017q2")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count17q3[, c("category", "docs2017q3")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count17q4[, c("category", "docs2017q4")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count18q1[, c("category", "docs2018q1")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count18q2[, c("category", "docs2018q2")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count18q3[, c("category", "docs2018q3")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count18q4[, c("category", "docs2018q4")], by="category", all.x=TRUE)

quarterlycounts$percent2014q4 <- as.numeric(quarterlycounts$docs2014q4) * 100 / nrow(filter(citycorp,datetimestamp <= '2014-12-31' & datetimestamp >= '2014-10-01'))
quarterlycounts$percent2015q1 <- as.numeric(quarterlycounts$docs2015q1) * 100 / nrow(filter(citycorp,datetimestamp <= '2015-3-31' & datetimestamp >= '2015-01-01'))
quarterlycounts$percent2015q2 <- as.numeric(quarterlycounts$docs2015q2) * 100 / nrow(filter(citycorp,datetimestamp <= '2015-06-30' & datetimestamp >= '2015-04-01'))
quarterlycounts$percent2015q3 <- as.numeric(quarterlycounts$docs2015q3) * 100 / nrow(filter(citycorp,datetimestamp <= '2015-09-30' & datetimestamp >= '2015-07-01'))
quarterlycounts$percent2015q4 <- as.numeric(quarterlycounts$docs2015q4) * 100 / nrow(filter(citycorp,datetimestamp <= '2015-12-31' & datetimestamp >= '2015-10-01'))
quarterlycounts$percent2016q1 <- as.numeric(quarterlycounts$docs2016q1) * 100 / nrow(filter(citycorp,datetimestamp <= '2016-3-31' & datetimestamp >= '2016-01-01'))
quarterlycounts$percent2016q2 <- as.numeric(quarterlycounts$docs2016q2) * 100 / nrow(filter(citycorp,datetimestamp <= '2016-06-30' & datetimestamp >= '2016-04-01'))
quarterlycounts$percent2016q3 <- as.numeric(quarterlycounts$docs2016q3) * 100 / nrow(filter(citycorp,datetimestamp <= '2016-09-30' & datetimestamp >= '2016-07-01'))
quarterlycounts$percent2016q4 <- as.numeric(quarterlycounts$docs2016q4) * 100 / nrow(filter(citycorp,datetimestamp <= '2016-12-31' & datetimestamp >= '2016-10-01'))
quarterlycounts$percent2017q1 <- as.numeric(quarterlycounts$docs2017q1) * 100 / nrow(filter(citycorp,datetimestamp <= '2017-3-31' & datetimestamp >= '2017-01-01'))
quarterlycounts$percent2017q2 <- as.numeric(quarterlycounts$docs2017q2) * 100 / nrow(filter(citycorp,datetimestamp <= '2017-06-30' & datetimestamp >= '2017-04-01'))
quarterlycounts$percent2017q3 <- as.numeric(quarterlycounts$docs2017q3) * 100 / nrow(filter(citycorp,datetimestamp <= '2017-09-30' & datetimestamp >= '2017-07-01'))
quarterlycounts$percent2017q4 <- as.numeric(quarterlycounts$docs2017q4) * 100 / nrow(filter(citycorp,datetimestamp <= '2017-12-31' & datetimestamp >= '2017-10-01'))
quarterlycounts$percent2018q1 <- as.numeric(quarterlycounts$docs2018q1) * 100 / nrow(filter(citycorp,datetimestamp <= '2018-3-31' & datetimestamp >= '2018-01-01'))
quarterlycounts$percent2018q2 <- as.numeric(quarterlycounts$docs2018q2) * 100 / nrow(filter(citycorp,datetimestamp <= '2018-06-30' & datetimestamp >= '2018-04-01'))
quarterlycounts$percent2018q3 <- as.numeric(quarterlycounts$docs2018q3) * 100 / nrow(filter(citycorp,datetimestamp <= '2018-09-30' & datetimestamp >= '2018-07-01'))
quarterlycounts$percent2018q4 <- as.numeric(quarterlycounts$docs2018q4) * 100 / nrow(filter(citycorp,datetimestamp <= '2018-12-31' & datetimestamp >= '2018-10-01'))

categorycount <- quarterlycounts %>%
  subset(category %in% termcategories) %>%
  select(c("category",starts_with("docs"))) %>%
  t()  %>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]
categorycounts <- mutate_all(as.data.frame(categorycount), function(x) as.numeric(as.character(x)))
categorycounts[is.na(categorycounts)] <- 0
allcategorycount <- cbind(categorycounts,quarters)

mosulcategories <- allcategorycount

longcategorycount <- mosulcategories %>%
  select(-no_category) %>%
  pivot_longer(any_of(termcategories),
               names_to="category",values_to="count")

ggplot(data=longcategorycount) +
  geom_line(aes(x=quarter,y=count,color=category)) +
  scale_color_manual(values = c("personnel_equipment_facilities" = "brown1",
                                "oil_infrastructure"="black",
                                "construction_equipment"="bisque",
                                "business_finance"="aquamarine",
                                "electrical_infrastructure"="darkgoldenrod1",
                                "transportation_infrastructure"="darkred",
                                "water_infrastructure"="cyan",
                                "terrain"="darkolivegreen",
                                "vehicles_not_military_oil"="darkslategray4",
                                "residential_buildings_other_critical_infrastructure"="burlywood4",
                                "manufacturing_production_industrial_infrastructure"="darkorchid1"
  )) +
  scale_x_date(date_breaks = "1 year") +
  xlab("Date") +
  ylab("Number of Press Releases Containing Category (Mosul)")

# 9 raqqa ----------------------------------------------------------------------

citycorp <- tidycorp %>%
  filter(location %in% c("raqqa"))

citytokens <- alltokens %>%
  filter(location %in% c("raqqa"))

citycount <- citytokens %>%
  count(category,sort=T)

# Quarterly

#2018
df18q1 <- citytokens %>%
  filter(datetimestamp <= '2018-03-31' & datetimestamp >= '2018-01-01')
count18q1 <- df18q1 %>%
  count(category,sort=T) %>%
  rename(docs2018q1 = n)
df18q2 <- citytokens %>%
  filter(datetimestamp <= '2018-06-30' & datetimestamp >= '2018-04-01')
count18q2 <- df18q2 %>%
  count(category,sort=T) %>%
  rename(docs2018q2 = n)
df18q3 <- citytokens %>%
  filter(datetimestamp <= '2018-09-30' & datetimestamp >= '2018-07-01')
count18q3 <- df18q3 %>%
  count(category,sort=T) %>%
  rename(docs2018q3 = n)
df18q4 <- citytokens %>%
  filter(datetimestamp <= '2018-12-31' & datetimestamp >= '2018-10-01')
count18q4 <- df18q4 %>%
  count(category,sort=T) %>%
  rename(docs2018q4 = n)
#2017
df17q1 <- citytokens %>%
  filter(datetimestamp <= '2017-03-31' & datetimestamp >= '2017-01-01')
count17q1 <- df17q1 %>%
  count(category,sort=T) %>%
  rename(docs2017q1 = n)
df17q2 <- citytokens %>%
  filter(datetimestamp <= '2017-06-30' & datetimestamp >= '2017-04-01')
count17q2 <- df17q2 %>%
  count(category,sort=T) %>%
  rename(docs2017q2 = n)
df17q3 <- citytokens %>%
  filter(datetimestamp <= '2017-09-30' & datetimestamp >= '2017-07-01')
count17q3 <- df17q3 %>%
  count(category,sort=T) %>%
  rename(docs2017q3 = n)
df17q4 <- citytokens %>%
  filter(datetimestamp <= '2017-12-31' & datetimestamp >= '2017-10-01')
count17q4 <- df17q4 %>%
  count(category,sort=T) %>%
  rename(docs2017q4 = n)
#2016
df16q1 <- citytokens %>%
  filter(datetimestamp <= '2016-03-31' & datetimestamp >= '2016-01-01')
count16q1 <- df16q1 %>%
  count(category,sort=T) %>%
  rename(docs2016q1 = n)
df16q2 <- citytokens %>%
  filter(datetimestamp <= '2016-06-30' & datetimestamp >= '2016-04-01')
count16q2 <- df16q2 %>%
  count(category,sort=T) %>%
  rename(docs2016q2 = n)
df16q3 <- citytokens %>%
  filter(datetimestamp <= '2016-09-30' & datetimestamp >= '2016-07-01')
count16q3 <- df16q3 %>%
  count(category,sort=T) %>%
  rename(docs2016q3 = n)
df16q4 <- citytokens %>%
  filter(datetimestamp <= '2016-12-31' & datetimestamp >= '2016-10-01')
count16q4 <- df16q4 %>%
  count(category,sort=T) %>%
  rename(docs2016q4 = n)
#2015 
df15q1 <- citytokens %>%
  filter(datetimestamp <= '2015-03-31' & datetimestamp >= '2015-01-01')
count15q1 <- df15q1 %>%
  count(category,sort=T) %>%
  rename(docs2015q1 = n)
df15q2 <- citytokens %>%
  filter(datetimestamp <= '2015-06-30' & datetimestamp >= '2015-04-01')
count15q2 <- df15q2 %>%
  count(category,sort=T) %>%
  rename(docs2015q2 = n)
df15q3 <- citytokens %>%
  filter(datetimestamp <= '2015-09-30' & datetimestamp >= '2015-07-01')
count15q3 <- df15q3 %>%
  count(category,sort=T) %>%
  rename(docs2015q3 = n)
df15q4 <- citytokens %>%
  filter(datetimestamp <= '2015-12-31' & datetimestamp >= '2015-10-01')
count15q4 <- df15q4 %>%
  count(category,sort=T) %>%
  rename(docs2015q4 = n)
#2014
df14q4 <- citytokens %>%
  filter(datetimestamp <= '2014-12-31' & datetimestamp >= '2014-10-01')
count14q4 <- df14q4 %>%
  count(category,sort=T) %>%
  rename(docs2014q4 = n)

quarterlycounts <- merge(citycount, count14q4[, c("category", "docs2014q4")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count15q1[, c("category", "docs2015q1")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count15q2[, c("category", "docs2015q2")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count15q3[, c("category", "docs2015q3")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count15q4[, c("category", "docs2015q4")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count16q1[, c("category", "docs2016q1")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count16q2[, c("category", "docs2016q2")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count16q3[, c("category", "docs2016q3")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count16q4[, c("category", "docs2016q4")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count17q1[, c("category", "docs2017q1")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count17q2[, c("category", "docs2017q2")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count17q3[, c("category", "docs2017q3")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count17q4[, c("category", "docs2017q4")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count18q1[, c("category", "docs2018q1")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count18q2[, c("category", "docs2018q2")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count18q3[, c("category", "docs2018q3")], by="category", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count18q4[, c("category", "docs2018q4")], by="category", all.x=TRUE)

quarterlycounts$percent2014q4 <- as.numeric(quarterlycounts$docs2014q4) * 100 / nrow(filter(citycorp,datetimestamp <= '2014-12-31' & datetimestamp >= '2014-10-01'))
quarterlycounts$percent2015q1 <- as.numeric(quarterlycounts$docs2015q1) * 100 / nrow(filter(citycorp,datetimestamp <= '2015-3-31' & datetimestamp >= '2015-01-01'))
quarterlycounts$percent2015q2 <- as.numeric(quarterlycounts$docs2015q2) * 100 / nrow(filter(citycorp,datetimestamp <= '2015-06-30' & datetimestamp >= '2015-04-01'))
quarterlycounts$percent2015q3 <- as.numeric(quarterlycounts$docs2015q3) * 100 / nrow(filter(citycorp,datetimestamp <= '2015-09-30' & datetimestamp >= '2015-07-01'))
quarterlycounts$percent2015q4 <- as.numeric(quarterlycounts$docs2015q4) * 100 / nrow(filter(citycorp,datetimestamp <= '2015-12-31' & datetimestamp >= '2015-10-01'))
quarterlycounts$percent2016q1 <- as.numeric(quarterlycounts$docs2016q1) * 100 / nrow(filter(citycorp,datetimestamp <= '2016-3-31' & datetimestamp >= '2016-01-01'))
quarterlycounts$percent2016q2 <- as.numeric(quarterlycounts$docs2016q2) * 100 / nrow(filter(citycorp,datetimestamp <= '2016-06-30' & datetimestamp >= '2016-04-01'))
quarterlycounts$percent2016q3 <- as.numeric(quarterlycounts$docs2016q3) * 100 / nrow(filter(citycorp,datetimestamp <= '2016-09-30' & datetimestamp >= '2016-07-01'))
quarterlycounts$percent2016q4 <- as.numeric(quarterlycounts$docs2016q4) * 100 / nrow(filter(citycorp,datetimestamp <= '2016-12-31' & datetimestamp >= '2016-10-01'))
quarterlycounts$percent2017q1 <- as.numeric(quarterlycounts$docs2017q1) * 100 / nrow(filter(citycorp,datetimestamp <= '2017-3-31' & datetimestamp >= '2017-01-01'))
quarterlycounts$percent2017q2 <- as.numeric(quarterlycounts$docs2017q2) * 100 / nrow(filter(citycorp,datetimestamp <= '2017-06-30' & datetimestamp >= '2017-04-01'))
quarterlycounts$percent2017q3 <- as.numeric(quarterlycounts$docs2017q3) * 100 / nrow(filter(citycorp,datetimestamp <= '2017-09-30' & datetimestamp >= '2017-07-01'))
quarterlycounts$percent2017q4 <- as.numeric(quarterlycounts$docs2017q4) * 100 / nrow(filter(citycorp,datetimestamp <= '2017-12-31' & datetimestamp >= '2017-10-01'))
quarterlycounts$percent2018q1 <- as.numeric(quarterlycounts$docs2018q1) * 100 / nrow(filter(citycorp,datetimestamp <= '2018-3-31' & datetimestamp >= '2018-01-01'))
quarterlycounts$percent2018q2 <- as.numeric(quarterlycounts$docs2018q2) * 100 / nrow(filter(citycorp,datetimestamp <= '2018-06-30' & datetimestamp >= '2018-04-01'))
quarterlycounts$percent2018q3 <- as.numeric(quarterlycounts$docs2018q3) * 100 / nrow(filter(citycorp,datetimestamp <= '2018-09-30' & datetimestamp >= '2018-07-01'))
quarterlycounts$percent2018q4 <- as.numeric(quarterlycounts$docs2018q4) * 100 / nrow(filter(citycorp,datetimestamp <= '2018-12-31' & datetimestamp >= '2018-10-01'))

categorycount <- quarterlycounts %>%
  subset(category %in% termcategories) %>%
  select(c("category",starts_with("docs"))) %>%
  t()  %>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]
categorycounts <- mutate_all(as.data.frame(categorycount), function(x) as.numeric(as.character(x)))
categorycounts[is.na(categorycounts)] <- 0
allcategorycount <- cbind(categorycounts,quarters)

raqqacategories <- allcategorycount

longcategorycount <- raqqacategories %>%
  select(-no_category) %>%
  pivot_longer(any_of(termcategories),
               names_to="category",values_to="count")

ggplot(data=longcategorycount) +
  geom_line(aes(x=quarter,y=count,color=category)) +
  scale_color_manual(values = c("personnel_equipment_facilities" = "brown1",
                                "oil_infrastructure"="black",
                                "construction_equipment"="bisque",
                                "business_finance"="aquamarine",
                                "electrical_infrastructure"="darkgoldenrod1",
                                "transportation_infrastructure"="darkred",
                                "water_infrastructure"="cyan",
                                "terrain"="darkolivegreen",
                                "vehicles_not_military_oil"="darkslategray4",
                                "residential_buildings_other_critical_infrastructure"="burlywood4",
                                "manufacturing_production_industrial_infrastructure"="darkorchid1")) +
  scale_x_date(date_breaks = "1 year") +
  xlab("Date") +
  ylab("Number of Press Releases Containing Category (Raqqa)")

