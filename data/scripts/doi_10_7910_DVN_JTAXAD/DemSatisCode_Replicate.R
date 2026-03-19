library(readr)
library(tidyverse)
library(tidytext)
library(stringr)
library(lubridate)
library(stm)
library(quanteda)
library(data.table)
library(scales)
library(stringr)
library(ggstance)
library(ggthemes)
library(forcats)
library(viridis)
library(ggmosaic)
library(reshape2)
library(stringi)
library(RColorBrewer)
library(vcd)
library(dplyr)
library(stopwords)
library(cowplot)
library(ca)
library(gplots)
library(stminsights)
setwd("C:/Users/han/Dropbox/Public/DemSatis")
# load --------------------------------------------------------------------
all_news <- readRDS("data/all_news.RDS")
all_articles <- readRDS("data/absolute_counted_articles.RDS")
# recoding CSES survey for S.Korea ---------------------------------------------------------
# select variables
load("data/cses_imd.rdata")
cses_all <- cses_imd %>% 
  filter(IMD1006_NAM == "Republic of Korea"
         ) %>% 
  select(IMD1008_YEAR,   #ELECTION YEAR
         IMD3005_3,      #PARTY IDENTIFICATION: WHO
         IMD3005_4,      #WHICH PARTY DO YOU FEEL CLOSEST TO
         IMD3006,        #LEFT-RIGHT - SELF
         IMD3008_A,      #LIKE-DISLIKE - PARTY A
         IMD3008_B,      #LIKE-DISLIKE - PARTY B
         IMD3008_C,      #LIKE-DISLIKE - PARTY C
         IMD3008_D,      #LIKE-DISLIKE - PARTY D
         IMD3008_E,      #LIKE-DISLIKE - PARTY E
         IMD3008_F,      #LIKE-DISLIKE - PARTY F
         IMD3010         #SATISFACTION WITH DEMOCRACY
          ) %>% 
  rename("year" = IMD1008_YEAR) %>% 
  mutate_if(is.integer, as.factor) %>% 
  as_tibble()
#recode IMD3005_3: partyID
cses_all <-　cses_all %>% 
  mutate(IMD3005_3 = recode(IMD3005_3,
      "4100001" = "New Frontier Party (NFP)",
      "4100002" = "Liberty Forward Party (LFP)",
      "4100003" = "New Progressive Party (NPP)",
      "4100004" = "Democratic United Party (DUP)",
      "4100005" = "Unified Progressive Party (UPP)",
      "4100006" = "Democratic Liberal Party", 
      "4100007" = "Pro-Park Geun-hye Alliance",
      "4100010" = "Millennium Democratic Party (MDP)",
      "4100011" = "United Liberal Democrats (ULD)",
      "4100012" = "Democratic People's Party",
      "4100015" = "Democratic Labor Party (DLP)",
      "4100016" = "Creative Korea Party (CKP)",
      "4100017" = "Renewal of Korea Party",
      "4100018" = "Democratic Party",
      "4100019" = "Korea Youth Progress Party",
      "4100020" = "National Integration 21",
      "4100021" = "Our Party",
      "4100022" = "Christian Party",
      "4100023" = "New Korean Party of the Hope (NKPH)",
      "9999988" = "NA",
      "9999989" = "NA",
      "9999990" = "NA", 
      "9999991" = "NA",
      "9999992" = "NA",
      "9999997" = "NA",
      "9999998" = "NA",
      "9999999" = "NA")) %>% 
  rename("partyID" = IMD3005_3) %>% 
  na_if("NA") %>% 
  drop_na()
#recode parties: ideological positions
cses_all$partyID_binary <- case_when(
  cses_all$partyID == "New Frontier Party (NFP)" ~ "Conservative",
  cses_all$partyID == "Our Party" ~ "Liberal",
  cses_all$partyID == "Democratic United Party (DUP)" ~ "Liberal",
  cses_all$partyID == "Millennium Democratic Party (MDP)" ~ "Liberal",
  cses_all$partyID == "Pro-Park Geun-hye Alliance" ~ "Conservative",
  cses_all$partyID == "Democratic Labor Party (DLP)" ~ "Progressive",
  cses_all$partyID == "Renewal of Korea Party" ~ "Liberal",
  cses_all$partyID == "Unified Progressive Party (UPP)" ~ "Progressive",
  cses_all$partyID == "Liberty Forward Party (LFP)" ~ "Conservative",
  cses_all$partyID == "New Progressive Party (NPP)" ~ "Progressive",
  cses_all$partyID == "United Liberal Democrats (ULD)" ~ "Conservative",
  cses_all$partyID == "Democratic People's Party" ~ "Conservative",
  cses_all$partyID == "Democratic Liberal Party" ~ "Liberal",
  cses_all$partyID == "National Integration 21" ~ "Progressive"
  ) 

# Ratio of satisfaction and liberal-progressives over time
# Fig 1.a Ratio of satisfaction over time
cses_all <- cses_all %>% 
  rename("self_position" = IMD3006) %>% 
  mutate(
    IMD3010 = recode(
      IMD3010,
      `1` = "VERY SATISFIED",
      `2` = "FAIRLY SATISFIED",
      `4` = "NOT VERY SATISFIED",
      `5` = "NOT AT ALL SATISFIED",
      `7` = NA_character_,
      `8` = NA_character_)) %>% 
  rename("democratic_satisfaction" = IMD3010)
 
cses_all %>% 
  drop_na() %>% 
  ggplot() + 
  geom_bar(aes(
    x = year, 
    fill = democratic_satisfaction),
    position = "fill") +
  labs(x = "Year",
       y = "Proportions",
       caption = "approx. 1000 responses per year") +
  scale_fill_viridis(option = "magma", discrete = TRUE) + 
  theme(legend.position="none") +
  theme_tufte(base_family = "sansserif") 

# Rename and recode like/dislike of parties for each year ---------------------------------------
cses_all <-　cses_all %>% 
  rename("like_party_A" = IMD3008_A) %>% 
  rename("like_party_B" = IMD3008_B) %>% 
  rename("like_party_C" = IMD3008_C) %>% 
  rename("like_party_D" = IMD3008_D) %>%
  rename("like_party_E" = IMD3008_E) %>%
  rename("like_party_F" = IMD3008_F) 

a <- cses_all %>% 
  dplyr::select(like_party_A, year, partyID_binary,
                self_position, democratic_satisfaction) %>% 
  drop_na() %>% 
  dplyr::mutate(whomst = case_when(
    year == 2000 ~ 
    "New Frontier Party (NFP)",
    year == 2004 ~ 
    "Our Party",
    year == 2008 ~ 
    "New Frontier Party (NFP)",
    year == 2012 ~ 
    "New Frontier Party (NFP)")) %>% 
  dplyr::mutate(pos = case_when(
    whomst == "New Frontier Party (NFP)" ~ "Conservative",
    whomst == "Our Party" ~ "Liberal",
    whomst == "Democratic United Party (DUP)" ~ "Liberal",
    whomst == "Millennium Democratic Party (MDP)" ~ "Liberal",
    whomst == "Pro-Park Geun-hye Alliance" ~ "Conservative",
    whomst == "Democratic Labor Party (DLP)" ~ "Progressive",
    whomst == "Renewal of Korea Party" ~ "Liberal",
    whomst == "Unified Progressive Party (UPP)" ~ "Progressive",
    whomst == "Liberty Forward Party (LFP)" ~ "Conservative",
    whomst == "New Progressive Party (NPP)" ~ "Progressive",
    whomst == "United Liberal Democrats (ULD)" ~ "Conservative",
    whomst == "Democratic People's Party" ~ "Conservative",
    whomst == "Democratic Liberal Party" ~ "Liberal",
    whomst == "National Integration 21" ~ "Progressive"
    )) %>% 
  arrange(desc(like_party_A)) %>% 
  filter(like_party_A %in% 0:10) %>% 
  dplyr::select(-whomst) %>% 
  dplyr::rename(like_partyID = like_party_A)

b <- cses_all %>% 
  dplyr::select(like_party_B, year, partyID_binary,
                self_position, democratic_satisfaction) %>% 
  dplyr::mutate(whomst = case_when(
    year == 2000 ~ 
      "Millennium Democratic Party (MDP)",
    year == 2004 ~ 
      "New Frontier Party (NFP)",
    year == 2008 ~ 
      "Democratic United Party (DUP)",
    year == 2012 ~ 
      "Democratic United Party (DUP)")) %>% 
  dplyr::mutate(pos = case_when(
    whomst == "New Frontier Party (NFP)" ~ "Conservative",
    whomst == "Our Party" ~ "Liberal",
    whomst == "Democratic United Party (DUP)" ~ "Liberal",
    whomst == "Millennium Democratic Party (MDP)" ~ "Liberal",
    whomst == "Pro-Park Geun-hye Alliance" ~ "Conservative",
    whomst == "Democratic Labor Party (DLP)" ~ "Progressive",
    whomst == "Renewal of Korea Party" ~ "Liberal",
    whomst == "Unified Progressive Party (UPP)" ~ "Progressive",
    whomst == "Liberty Forward Party (LFP)" ~ "Conservative",
    whomst == "New Progressive Party (NPP)" ~ "Progressive",
    whomst == "United Liberal Democrats (ULD)" ~ "Conservative",
    whomst == "Democratic People's Party" ~ "Conservative",
    whomst == "Democratic Liberal Party" ~ "Liberal",
    whomst == "National Integration 21" ~ "Progressive")) %>% 
  arrange(desc(like_party_B)) %>% 
  dplyr::filter(like_party_B %in% 0:10) %>% 
  dplyr::select(-whomst) %>% 
  dplyr::rename(like_partyID = like_party_B)

c <- cses_all %>% 
  dplyr::select(like_party_C, year, partyID_binary,
                self_position, democratic_satisfaction) %>% 
  dplyr::mutate(whomst = case_when(
    year == 2000 ~ 
      "United Liberal Democrats (ULD)",
    year == 2004 ~ 
      "Democratic Labor Party (DLP)",
    year == 2008 ~ 
      "Liberty Forward Party (LFP)",
    year == 2012 ~ 
      "Unified Progressive Party (UPP)")) %>% 
  dplyr::mutate(pos = case_when(
    whomst == "New Frontier Party (NFP)" ~ "Conservative",
    whomst == "Our Party" ~ "Liberal",
    whomst == "Democratic United Party (DUP)" ~ "Liberal",
    whomst == "Millennium Democratic Party (MDP)" ~ "Liberal",
    whomst == "Pro-Park Geun-hye Alliance" ~ "Conservative",
    whomst == "Democratic Labor Party (DLP)" ~ "Progressive",
    whomst == "Renewal of Korea Party" ~ "Liberal",
    whomst == "Unified Progressive Party (UPP)" ~ "Progressive",
    whomst == "Liberty Forward Party (LFP)" ~ "Conservative",
    whomst == "New Progressive Party (NPP)" ~ "Progressive",
    whomst == "United Liberal Democrats (ULD)" ~ "Conservative",
    whomst == "Democratic People's Party" ~ "Conservative",
    whomst == "Democratic Liberal Party" ~ "Liberal",
    whomst == "National Integration 21" ~ "Progressive")) %>% 
  arrange(desc(like_party_C)) %>% 
  dplyr::filter(like_party_C %in% 0:10) %>% 
  dplyr::select(-whomst) %>% 
  dplyr::rename(like_partyID = like_party_C)

d <- cses_all %>% 
  dplyr::select(like_party_D, year, partyID_binary,
                self_position, democratic_satisfaction) %>% 
  dplyr::mutate(whomst = case_when(
    year == 2000 ~ 
      "Democratic People's Party",
    year == 2004 ~ 
      "Millennium Democratic Party (MDP)",
    year == 2008 ~ 
      "Pro-Park Geun-hye Alliance",
    year == 2012 ~ 
      "Liberty Forward Party (LFP)")) %>% 
  dplyr::mutate(pos = case_when(
    whomst == "New Frontier Party (NFP)" ~ "Conservative",
    whomst == "Our Party" ~ "Liberal",
    whomst == "Democratic United Party (DUP)" ~ "Liberal",
    whomst == "Millennium Democratic Party (MDP)" ~ "Liberal",
    whomst == "Pro-Park Geun-hye Alliance" ~ "Conservative",
    whomst == "Democratic Labor Party (DLP)" ~ "Progressive",
    whomst == "Renewal of Korea Party" ~ "Liberal",
    whomst == "Unified Progressive Party (UPP)" ~ "Progressive",
    whomst == "Liberty Forward Party (LFP)" ~ "Conservative",
    whomst == "New Progressive Party (NPP)" ~ "Progressive",
    whomst == "United Liberal Democrats (ULD)" ~ "Conservative",
    whomst == "Democratic People's Party" ~ "Conservative",
    whomst == "Democratic Liberal Party" ~ "Liberal",
    whomst == "National Integration 21" ~ "Progressive")) %>% 
  arrange(desc(like_party_D)) %>% 
  dplyr::filter(like_party_D %in% 0:10) %>% 
  dplyr::select(-whomst) %>% 
  dplyr::rename(like_partyID = like_party_D)

e <- cses_all %>% 
  dplyr::select(like_party_E, year, partyID_binary,
                self_position, democratic_satisfaction) %>% 
  dplyr::mutate(whomst = case_when(
    year == 2000 ~ 
      "Democratic Liberal Party",
    year == 2004 ~ 
      "United Liberal Democrats (ULD)",
    year == 2008 ~ 
      "Democratic Labor Party (DLP)",
    year == 2012 ~ 
      "Christian Party")) %>% 
  dplyr::mutate(pos = case_when(
    whomst == "New Frontier Party (NFP)" ~ "Conservative",
    whomst == "Our Party" ~ "Liberal",
    whomst == "Democratic United Party (DUP)" ~ "Liberal",
    whomst == "Millennium Democratic Party (MDP)" ~ "Liberal",
    whomst == "Pro-Park Geun-hye Alliance" ~ "Conservative",
    whomst == "Democratic Labor Party (DLP)" ~ "Progressive",
    whomst == "Renewal of Korea Party" ~ "Liberal",
    whomst == "Unified Progressive Party (UPP)" ~ "Progressive",
    whomst == "Liberty Forward Party (LFP)" ~ "Conservative",
    whomst == "New Progressive Party (NPP)" ~ "Progressive",
    whomst == "United Liberal Democrats (ULD)" ~ "Conservative",
    whomst == "Democratic People's Party" ~ "Conservative",
    whomst == "Democratic Liberal Party" ~ "Liberal",
    whomst == "National Integration 21" ~ "Progressive",
    whomst == "Christian Party" ~ "Conservative")) %>% 
  arrange(desc(like_party_E)) %>% 
  dplyr::filter(like_party_E %in% 0:10) %>% 
  dplyr::select(-whomst) %>% 
  dplyr::rename(like_partyID = like_party_E)

f <- cses_all %>% 
  dplyr::select(like_party_F, year, partyID_binary,
                self_position, democratic_satisfaction) %>% 
  dplyr::mutate(whomst = case_when(
    year == 2000 ~ 
      "New Korean Party of the Hope (NKPH)",
    year == 2004 ~ 
      "National Integration 21",
    year == 2008 ~ 
      "Renewal of Korea Party",
    year == 2012 ~ 
      "New Progressive Party (NPP)")) %>% 
  dplyr::mutate(pos = case_when(
    whomst == "New Frontier Party (NFP)" ~ "Conservative",
    whomst == "Our Party" ~ "Liberal",
    whomst == "Democratic United Party (DUP)" ~ "Liberal",
    whomst == "Millennium Democratic Party (MDP)" ~ "Liberal",
    whomst == "Pro-Park Geun-hye Alliance" ~ "Conservative",
    whomst == "Democratic Labor Party (DLP)" ~ "Progressive",
    whomst == "Renewal of Korea Party" ~ "Liberal",
    whomst == "Unified Progressive Party (UPP)" ~ "Progressive",
    whomst == "Liberty Forward Party (LFP)" ~ "Conservative",
    whomst == "New Progressive Party (NPP)" ~ "Progressive",
    whomst == "United Liberal Democrats (ULD)" ~ "Conservative",
    whomst == "Democratic People's Party" ~ "Conservative",
    whomst == "Democratic Liberal Party" ~ "Liberal",
    whomst == "National Integration 21" ~ "Progressive",
    whomst == "Christian Party" ~ "Conservative",
    whomst == "New Korean Party of the Hope (NKPH)" ~ "Conservative"
    )) %>% 
  arrange(desc(like_party_F)) %>% 
  dplyr::filter(like_party_F %in% 0:10) %>% 
  dplyr::select(-whomst) %>% 
  dplyr::rename(like_partyID = like_party_F) 

liking <- rbind(a, b, c, d, e, f) %>% 
  filter(self_position %in% 0:10)
liking[ liking == "NA" ] <- NA
liking <- liking %>% drop_na()

rm(a, b, c, d, e, f)

# Recode variables and filter values ----------------------------------------------
a <- liking %>% 
  dplyr::filter(like_partyID == 0 | like_partyID == 10) %>% 
  droplevels %>% 
  dplyr::select(year, democratic_satisfaction, like_partyID, pos,
                self_position, partyID_binary) %>% 
  dplyr::mutate(democratic_satisfaction = case_when(
    democratic_satisfaction == "VERY SATISFIED" ~ "Very",
    democratic_satisfaction == "FAIRLY SATISFIED" ~ "Fairly",
    democratic_satisfaction == "NOT VERY SATISFIED" ~ "Not very",
    democratic_satisfaction == "NOT AT ALL SATISFIED" ~ "Not at all")) %>%
  dplyr::mutate(demsatis_2 = case_when(
    democratic_satisfaction == "Very" ~ "Satisfied: Yes",
    democratic_satisfaction == "Fairly" ~ "Satisfied: Yes",
    democratic_satisfaction == "Not very" ~ "Satisfied: No",
    democratic_satisfaction == "Not at all" ~ "Satisfied: No")) %>%
  dplyr::mutate(demsatis_3 = case_when(
    demsatis_2 == "Satisfied: Yes" ~ "Yes",
    demsatis_2 == "Satisfied: Yes" ~ "Yes",
    demsatis_2 == "Satisfied: No" ~ "No",
    demsatis_2 == "Satisfied: No" ~ "No")) %>%
  dplyr::mutate(govt = case_when(
    year == "2000" ~ "2000 \nLiberal \ngovernment",
    year == "2004" ~ "2004 \nLiberal \ngovernment",
    year == "2008" ~ "2008 \nConservative \ngovernment",
    year == "2012" ~ "2012 \nConservative \ngovernment")) %>%
  dplyr::mutate(govt_short = case_when(
    year == "2000" ~ "Liberal government",
    year == "2004" ~ "Liberal government",
    year == "2008" ~ "Conservative government",
    year == "2012" ~ "Conservative government")) %>%
  dplyr::mutate(partyID_2 = case_when(
    partyID_binary == "Conservative" ~ "Identify with \nConservative party",
    partyID_binary == "Progressive" ~ "Identify with \nProgressive \nor Liberal party",
    partyID_binary == "Liberal" ~ "Identify with \nProgressive \nor Liberal party")) %>% 
  dplyr::mutate(self_simple = case_when(
    self_position %in% 0:3 ~ "Left",
    self_position %in% 4:6 ~ "Center",
    self_position %in% 7:10 ~ "Right")) %>% 
  mutate_if(is.character, as_factor) 
#relevel factor levels
a$like_partyID <- factor(a$like_partyID,
                         levels = c("0", "10"),
                         labels = c("Strongly dislike",
                                    "Strongly like"))
a$democratic_satisfaction <- fct_relevel(a$democratic_satisfaction,
                                         "Very", "Fairly", "Not very")
a$self_simple <- fct_relevel(a$self_simple, "Left", "Center")                                  
a$pos <- fct_relevel(a$pos, "Progressive", "Liberal")
a$partyID_binary <- fct_relevel(a$partyID_binary, 
                                "Progressive", "Liberal")
a$partyID_2 <- fct_relevel(a$partyID_2, "Progressive", "Liberal")
a$demsatis_3 <- fct_relevel(a$demsatis_3, "Yes", "No")

# main analyses -----------------------------------------------------------
# Fig 2. Abstract partisanship
#use all 1-10
ggplot(a) +
  geom_bar(aes(x = self_position, fill = demsatis_3),
           position = "fill") +
  labs(x = "Respondents' self-placement from Left to Right",
       y = "Ratio",
       caption = "CSES IMD survey items from the years 2000, 2004, 2008, 2012; 1656 observations
       Governments: Liberal = 2004 and 2008, Conservative = 2000 and 2012
       Respondents' self-placement on a 0-10 Left-Right scale (IMD3006); Most Left = 0, Most Right = 10
       Satisfaction with democracy (IMD3010)",
       fill = "Satisfaction \nwith democracy") +
  facet_grid(.~govt_short) +
  scale_fill_viridis_d(end = .6) +
  theme_classic()
ggsave("plots/abstractbias.pdf", width = 7, height = 2.5)

# Fig 3. Affective partisanship
a <- a %>% 
   dplyr::mutate(incumb = case_when(
     pos == "Conservative" & like_partyID == "Strongly dislike" & govt_short == "Conservative government" ~ "Dislike incumbent",
     pos == "Liberal" & like_partyID == "Strongly dislike" & govt_short == "Liberal government" ~ "Dislike incumbent",
     pos == "Progressive"& like_partyID == "Strongly dislike" & govt_short == "Liberal government" ~ "Dislike incumbent",
     pos == "Conservative" & like_partyID == "Strongly like" & govt_short == "Conservative government" ~ "Like incumbent",
     pos == "Liberal" & like_partyID == "Strongly like" & govt_short == "Liberal government" ~ "Like incumbent",
     pos == "Progressive"& like_partyID == "Strongly like" & govt_short == "Liberal government" ~ "Like incumbent",
     pos == "Conservative" & like_partyID == "Strongly like" & govt_short == "Liberal government" ~ "Like opposition",
     pos == "Liberal" & like_partyID == "Strongly like" & govt_short == "Conservative government" ~ "Like opposition",
     pos == "Progressive"& like_partyID == "Strongly like" & govt_short == "Conservative government" ~ "Like opposition",
     pos == "Conservative" & like_partyID == "Strongly dislike" & govt_short == "Liberal government" ~ "Dislike opposition",
     pos == "Liberal" & like_partyID == "Strongly dislike" & govt_short == "Conservative government" ~ "Dislike opposition",
     pos == "Progressive"& like_partyID == "Strongly dislike" & govt_short == "Conservative government" ~ "Dislike opposition"
     )) %>% 
  dplyr::mutate(incumb_ID = case_when(
    partyID_binary == "Conservative" & govt_short == "Conservative government" ~ "Identify with incumbent",
    partyID_binary== "Liberal" & govt_short == "Liberal government" ~ "Identify with incumbent",
    partyID_binary== "Progressive"& govt_short == "Liberal government" ~ "Identify with incumbent",
    partyID_binary== "Conservative" & govt_short == "Liberal government" ~ "Identify with opposition",
    partyID_binary== "Liberal" & govt_short == "Conservative government" ~ "Identify with opposition",
    partyID_binary== "Progressive"& govt_short == "Conservative government" ~ "Identify with opposition"
  )) 
a$incumb <- fct_relevel(b$incumb, "Dislike incumbent", "Like opposition")
a$incumb2 <- fct_collapse(b$incumb,
                          losers  =  c("Dislike incumbent", "Like opposition"), 
                          winners = c("Like incumbent", "Dislike opposition"))
ggplot(a) +
  geom_mosaic(aes(x = product(incumb2), 
                  fill= demsatis_3), offset = 0.02) +
  facet_wrap(govt_short~.) +
  labs(x = "Winners/losers based on like/dislike of incumbent/opposition party",
       y = "Satisfaction \nwith democracy",
       caption = "CSES IMD survey items from the years 2000, 2004, 2008, 2012 (1656 observations)
       Governments: Liberal = 2004 and 2008, Conservative = 2000 and 2012
       Likeability of Parties A-I on a 0-10 scale (IMD3008); Strongly dislike = 0, Strongly like = 10
       Parties A-I differ by year and were labeled as Conservative/ Liberal/ Progressive
       losers = dislike incumbent party + like opposition party, 
       winners = like incumbent party + dislike opposition party
       Satisfaction with democracy (IMD3010); very/ fairly = Yes, not very/not at all = No",
       fill = "Satisfaction \nwith \ndemocracy"
  ) + 
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "plasma", end = .7) 
ggsave("plots/dislike_incumbent.pdf", height = 3, width = 5.5)

# Social partisanship
# Fig 4. Party identification and satisfaction
ggplot(a) +
  geom_mosaic(aes(x = product(partyID_2), 
                  fill=demsatis_3 ),
              divider = ddecker(), offset = 0.02) +
  facet_grid(.~govt_short) +
  labs(x = "Party identification",
       y = "Satisfaction \nwith democracy",
       caption = "CSES IMD survey items from the years 2000, 2004, 2008, 2012 (1656 observations)
       Governments: Liberal = 2004 and 2008, Conservative = 2000 and 2012
       Party identification (IMD3005_3); Which party do you feel closest to?
       Satisfaction with democracy (IMD3010); very/ fairly = Yes, not very/not at all = No",
       fill = "Satisfaction \nwith democracy"
       ) + 
  theme_classic() +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "magma", end = 0.6)

# Fig 5. partian sorting over time 
ggplot(a) +
  geom_mosaic(aes(x = product(self_simple), 
                  fill=partyID_2), offset = 0.02) +
  facet_grid(.~year, drop = TRUE, scales = "free_x") +
  scale_fill_viridis_d(option = "B", end = .9) +
  labs(x = "Self-placement",
       y = "Party identification",
       caption = "CSES IMD survey items from the years 2000, 2004, 2008, 2012 (1656 observations total)
       Party identification (IMD3005_3): Which party do you feel closest to?
       Self-placement on a 0-10 scale (IMD3006); Left = 0:3, Center = 4:6, Right = 7:10
       ") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(legend.position = "none") 

# Fig 6. Correspondence of partisan sorting variables
par(mar=c(5.1, 4.1, 4.1, 4.1), xpd=NA)
mytable <- with(a, table(self_simple,
                         partyID_binary)) # create a 2 way table
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages
fit <- ca(mytable)
print(fit) # basic results 
summary(fit) # extended results 
plot(fit, mass = TRUE, contrib = "absolute", map =
       "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map

# Fig 7. Partisan sorting and satisfaction by incumbency
ggplot(a) +
  geom_mosaic(aes(x = product(self_simple), 
                  fill=incumb_ID), offset = 0.02) +
  facet_grid(demsatis_2~govt, drop = TRUE, scales = "free_x") +
  scale_fill_viridis_d(option = "B") +
  labs(x = "Self-placement from Left to Right",
       y = "Party identification",
       caption = "CSES IMD survey items from the years 2000, 2004, 2008, 2012 (1656 observations total)
       Party identification (IMD3005_3): Which party do you feel closest to?
       Respondents' self-placement on a 0-10 scale (IMD3006); Leftist = 0:3, Centrist = 4:6, Rightist = 7:10
       ") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(legend.position = "none") 

# Validations -------------------------------------------------------------
# Fig 1.a Ratio of satisfaction over time 
ggplot(a) +
  geom_bar(aes(x = year, fill = democratic_satisfaction),
           position = "fill") +
  labs(x = "Year",
       y = "Ratio",
       fill = "Satisfaction \nwith democracy") +
  scale_fill_brewer(palette = "PiYG", direction = -1) +
  theme_classic()
# Fig 1.b Ratio of liberal-progressives over time  
a %>% 
  dplyr::select(year, partyID_binary) %>% 
  dplyr::group_by(year) %>% 
  dplyr::count(partyID_binary) %>% 
ggplot(aes(x= year, weights = n, fill = partyID_binary)) + 
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Pastel1", direction = -1) +
  theme_classic() +
  labs(x = "Year",
       y = "Count",
       fill = "Party identification \nby respondents") 

# stm: find topics --------------------------------------------------------------
# save legit stopwords, and add some
stopwords <- tibble(
  tokens = stopwords::stopwords("ko", source = "stopwords-iso"),
  lexicon = "stopwords-iso") 
stopwords <- stopwords %>% 
  add_row(tokens = c("들이", "하기", "그것", "때문"), 
          lexicon = "stopwords-iso")

# pass matrix from tidy to stm 
tidy_news <- all_news %>%
  dplyr::select(tokens, Newspaper, Date, Body, Prezparty) %>% 
  dplyr::mutate(article =  row_number()) %>%
  tidyr::unnest(tokens) %>% 
  dplyr::anti_join(stopwords) %>% 
  dplyr::filter(tokens != "민주주의") 
news_sparse <- tidy_news %>%
  dplyr::count(article, tokens) %>%
  cast_sparse(article, tokens, n)
covariates <- tidy_news %>%
  distinct(article, Newspaper, Date, Body, Prezparty) 

covariates$Prezparty <- as.factor(covariates$Prezparty)
covariates$Date <- format(covariates$Date, "%Y%m%d")
covariates$Date <- as.numeric(covariates$Date)
covariates$Date <- ymd(covariates$Date)

#fit a model
topic_model <- stm(news_sparse, 
                   K = 12, 
                   prevalence = ~ Newspaper +s(Date),
                   data = covariates,
                   verbose = FALSE, 
                   init.type = "Spectral")


#understand the model output
labelTopics(topic_model)
plot(topic_model, type = "summary")
plot(topic_model, type = "labels")
plot(topic_model, type = "hist")

thoughts1 <- findThoughts(topic_model, texts = covariates$Body, 
                          n = 2, topics = 1)
thoughts2 <- findThoughts(topic_model, texts = covariates$Body, 
                          n = 2, topics = 2)
thoughts3 <- findThoughts(topic_model, texts = covariates$Body, 
                          n = 2, topics = 3)
thoughts4 <- findThoughts(topic_model, texts = covariates$Body, 
                          n = 2, topics = 4)
thoughts5 <- findThoughts(topic_model, texts = covariates$Body, 
                          n = 2, topics = 5)
thoughts6 <- findThoughts(topic_model, texts = covariates$Body, 
                          n = 2, topics = 6)
thoughts7 <- findThoughts(topic_model, texts = covariates$Body, 
                          n = 2, topics = 7)
thoughts8 <- findThoughts(topic_model, texts = covariates$Body, 
                          n = 2, topics = 8)
thoughts9 <- findThoughts(topic_model, texts = covariates$Body, 
                          n = 2, topics = 9)
thoughts10 <- findThoughts(topic_model, texts = covariates$Body, 
                          n = 2, topics = 10)
thoughts11 <- findThoughts(topic_model, texts = covariates$Body, 
                          n = 2, topics = 11)
thoughts12 <- findThoughts(topic_model, texts = covariates$Body, 
                          n = 2, topics = 12)
par(mfrow = c(2, 6),mar = c(.5, .5, 1, .5))
plotQuote(thoughts1$docs[[1]] %>% word(1, 30), 
          width = 30, main = "Topic 1")
plotQuote(thoughts2$docs[[1]] %>% word(1, 30), 
          width = 30, main = "Topic 2")
plotQuote(thoughts3$docs[[1]] %>% word(1, 30), 
          width = 30, main = "Topic 3")
plotQuote(thoughts4$docs[[1]] %>% word(1, 30), 
          width = 30, main = "Topic 4")
plotQuote(thoughts5$docs[[1]] %>% word(1, 30), 
          width = 30, main = "Topic 5")
plotQuote(thoughts6$docs[[1]] %>% word(1, 30), 
          width = 30, main = "Topic 6")
plotQuote(thoughts7$docs[[1]] %>% word(1, 30), 
          width = 30, main = "Topic 7")
plotQuote(thoughts8$docs[[1]] %>% word(1, 30), 
          width = 30, main = "Topic 8")
plotQuote(thoughts9$docs[[1]] %>% word(1, 30), 
          width = 30, main = "Topic 9")
plotQuote(thoughts10$docs[[1]] %>% word(1, 30), 
          width = 30, main = "Topic 10")
plotQuote(thoughts11$docs[[1]] %>% word(1, 30), 
          width = 30, main = "Topic 11")
plotQuote(thoughts12$docs[[1]] %>% word(1, 30), 
          width = 30, main = "Topic 12")

par(mfrow = c(1, 1))
plot(topic_model, type="perspectives", topics=c(1,9))
plot(topic_model, type="perspectives", topics=c(3,10))
plot(topic_model, type="perspectives", topics=c(2,4))
plot(topic_model, type="perspectives", topics=c(12,7))
plot(topic_model, type="perspectives", topics=c(6,5))
plot(topic_model, type="perspectives", topics=c(8,11))

topic_labels <- c(
  "12" = "Topic: Global values, human rights",
  "11" = "Topic: Demonstration and protest",
  "10" = "Topic: International democratizations",
  "9" = "Topic: Sentimental humanism",
  "8" = "Topic: Government",
  "7" = "Topic: The U.S.",
  "6" = "Topic: Political prosecution",
  "5" = "Topic: Party politics",
  "4" = "Topic: North Korea",
  "3" = "Topic: Public opinion, media",
  "2" = "Topic: Civil society, education",
  "1" = "Topic: Ideology and ideals")
# stm interaction effects ----------------------------------------------------
# combine estimates for interaction effects
prep_int <- estimateEffect(1:12 ~ Newspaper * s(Date),
                           topic_model, covariates)
effects_int <- get_effects(estimates = prep_int,
                           variable = 'Date',
                           type = 'continuous',
                           moderator = 'Newspaper',
                           modval = "Chosun") %>%
  bind_rows(get_effects(estimates = prep_int,
                variable = 'Date',
                type = 'continuous',
                moderator = 'Newspaper',
                modval = "Hankook"),
            get_effects(estimates = prep_int,
                        variable = 'Date',
                        type = 'continuous',
                        moderator = 'Newspaper',
                        modval = "Hankyoreh"))

labels <- c("Chosun" = "Conservative", "Hankyoreh" = "Liberal/\nProgressive")

effects_int %>% filter(topic == 7) %>%
  filter(moderator != "Hankook") %>% 
  mutate(moderator = as.factor(moderator)) %>%
  ggplot(aes(x = value, y = proportion, color = moderator,
             fill = moderator)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  theme_light() + labs(x = 'Year', y = 'Topic Proportion',
                       subtitle = 'Topic 7: The U.S.') +
  scale_x_continuous(breaks = yearseq2, labels = yearnames) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = c(19980000, 20080000),  
             linetype="dashed", size = .5) +
  annotate("text", x = 19940000, label = "Conservative", y = 0.015,
           size = 3) +
  annotate("text", x = 20030000, label = "Liberal\nProgressive", y = 0.015,
           size = 3) +
  annotate("text", x = 20120000, label = "Conservative ", y = 0.015,
           size = 3) +
  scale_fill_discrete(name = "Newspapers",
                      labels = labels) +
  scale_color_discrete(name = "Newspapers",
                       labels = labels)

# analysis stm ------------------------------------------------------------
#plot multiple: North Korea
effects_int %>% filter(topic == c(4,8)) %>%
  filter(moderator != "Hankook") %>% 
  mutate(moderator = as.factor(moderator)) %>%
  ggplot(aes(x = value, y = proportion, color = moderator,
             fill = moderator)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  theme_classic() + 
  labs(x = 'Year', y = 'Topic Proportion',
       caption = 
         "Two major newspapers: Chosun Ilbo, Hankyoreh. Articles contain keyword 'democracy', 1990-2014
      *Liberals dissatisfied = Leftist partisan dissatisfaction with democracy (source: CSES, 2004 wave)
      ") +
  scale_x_continuous(breaks = c(20000000, 20040000, 20060000), 
                     labels = c(2000, 2004, 2006)) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
   annotate("rect", 
           xmin = 20040000, xmax = 20050000,
           ymin = 0, ymax = .2, 
           alpha = .5, fill = "yellow") +
  annotate("rect", 
           xmin = 20000000, xmax = 20010000,
           ymin = 0, ymax = .2, 
           alpha = .6, fill = "pink") +
  annotate("rect", 
           xmin = 20060000, xmax = 20070000,
           ymin = 0, ymax = .2, 
           alpha = .6, fill = "pink") +
  annotate("text", x = 20000000, label = "Inter-Korean Summit",
           y = 0.14, size = 3, angle = 90) +
  annotate("text", x = 20060000, label = "Nuclear test",
           y = 0.16, size = 3, angle = 90) +
  annotate("text", x = 20040000, label = "Liberals dissatisfied*", 
           y = 0.14, size = 3, angle = 90) +
 scale_fill_discrete(name = "Newspapers",
                      labels = labels) +
  scale_color_discrete(name = "Newspapers",
                       labels = labels) +
  facet_grid(.~topic, labeller = labeller(topic = topic_labels))
ggsave("plots/stm_int_effect_NoKo.pdf", width = 6.5, height = 4)

#plot multiple: Impeachment
effects_int %>% filter(topic == c(5,6)) %>%
  filter(moderator != "Hankook") %>% 
  mutate(moderator = as.factor(moderator)) %>%
  ggplot(aes(x = value, y = proportion, color = moderator,
             fill = moderator)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  theme_classic() + 
  labs(x = 'Year', y = 'Topic Proportion',
       caption = 
         "Two major newspapers: Chosun Ilbo, Hankyoreh. Articles contain keyword 'democracy', 1990-2014
      *Liberals dissatisfied = Leftist partisan dissatisfaction with democracy (source: CSES, 2004 wave)
     ") +
  scale_x_continuous(breaks = c(19980000, 20040000, 20080000), 
                     labels = c(1998, 2004, 2008)) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  annotate("rect", 
           xmin = 19980000, xmax = 20080000,
           ymin = 0, ymax = .2, 
           alpha = .2, fill = "grey") +
  annotate("rect", 
           xmin = 20040000, xmax = 20050000,
           ymin = 0, ymax = .2, 
           alpha = .5, fill = "yellow") +
  annotate("text", x = 19990000, label = "Liberal government", 
           y = 0.14, size = 3, angle = 90) +
  annotate("text", x = 20040000, label = "Liberals dissatisfied*", 
           y = 0.14, size = 3, angle = 90) +
  annotate("pointrange", x = 20040000, y = .03, ymin = .03, ymax = .07,
           colour = "red", size = 0.5) +
  annotate("text", x = 20040000, label = "Impeachment",
           y = .01, colour = "red", size = 3) +
  scale_fill_discrete(name = "Newspapers",
                      labels = labels) +
  scale_color_discrete(name = "Newspapers",
                       labels = labels) +
  facet_grid(.~topic, labeller = labeller(topic = topic_labels))
ggsave("plots/stm_int_effect_impeachment.pdf", width = 6.5, height = 4)

#plot multiple: FTA
effects_int %>% filter(topic == c(7,11)) %>%
  filter(moderator != "Hankook") %>% 
  mutate(moderator = as.factor(moderator)) %>%
  ggplot(aes(x = value, y = proportion, color = moderator,
             fill = moderator)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  theme_classic() + 
  labs(x = 'Year', y = 'Topic Proportion',
       caption = 
         "Two major newspapers: Chosun Ilbo, Hankyoreh. Articles contain keyword 'democracy', 1990-2014
      *Liberals dissatisfied = Leftist partisan dissatisfaction with democracy (source: CSES, 2004 wave)
       ") +
  scale_x_continuous(breaks = c(19980000, 20040000, 20080000), 
                     labels = c(1998, 2004, 2008)) +
  scale_y_continuous(limits = c(0,0.25)) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  annotate("rect", 
           xmin = 19980000, xmax = 20080000,
           ymin = 0, ymax = .25, 
           alpha = .2, fill = "grey") +
  annotate("rect", 
           xmin = 20040000, xmax = 20050000,
           ymin = 0, ymax = .25, 
           alpha = .5, fill = "yellow") +
  annotate("rect", 
           xmin = 20080000, xmax = 20090000,
           ymin = 0, ymax = .25, 
           alpha = .7, fill = "lightblue") +
  annotate("text", x = 19990000, label = "Liberal government", 
           y = 0.18, size = 3, angle = 90) +
  annotate("text", x = 20040000, label = "Liberals dissatisfied*", 
           y = 0.18, size = 3, angle = 90) +
  annotate("text", x = 20080000, label = "U.S. Beef protest", 
           y = 0.18, size = 3, angle = 90) +
  scale_fill_discrete(name = "Newspapers",
                      labels = labels) +
  scale_color_discrete(name = "Newspapers",
                       labels = labels) +
  facet_grid(.~topic, labeller = labeller(topic = topic_labels)) 
ggsave("plots/stm_int_effect_FTA.pdf", width = 6.5, height = 4)

#plot multiple: Norms
effects_int %>% filter(topic == c(10,12)) %>%
  filter(moderator != "Hankook") %>% 
  mutate(moderator = as.factor(moderator)) %>%
  ggplot(aes(x = value, y = proportion, color = moderator,
             fill = moderator)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  theme_classic() + 
  labs(x = 'Year', y = 'Topic Proportion',
       caption = 
         "Two major newspapers: Chosun Ilbo, Hankyoreh. Articles contain keyword 'democracy', 1990-2014
       ") +
  scale_x_continuous(breaks = c(19900000, 19940000, 20000000, 20070000), 
                     labels = c(1990, 1994, 2000, 2007)) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  annotate("rect", 
           xmin = 19900000, xmax = 19940000,
           ymin = 0, ymax = .25, 
           alpha = .2, fill = "orange") +
  annotate("rect", 
           xmin = 20000000, xmax = 20070000,
           ymin = 0, ymax = .25, 
           alpha = .3, fill = "lightblue") +
  annotate("text", x = 19920000, label = "Post-soviet wave",
          y = 0.18, size = 3, angle = 90) +
  annotate("text", x = 20030000, label = "Color revolution",
           y = 0.18, size = 3, angle = 90) +
  scale_fill_discrete(name = "Newspapers",
                      labels = labels) +
  scale_color_discrete(name = "Newspapers",
                       labels = labels) +
  facet_grid(.~topic, labeller = labeller(topic = topic_labels))
ggsave("plots/stm_int_effect_norms.pdf", width = 6.5, height = 4)

#plot multiple: associations
effects_int %>% filter(topic == c(1,9)) %>%
  filter(moderator != "Hankook") %>% 
  mutate(moderator = as.factor(moderator)) %>%
  ggplot(aes(x = value, y = proportion, color = moderator,
             fill = moderator)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  theme_classic() + 
  labs(x = 'Year', y = 'Topic Proportion',
       caption = 
         "Two major newspapers: Chosun Ilbo, Hankyoreh. Articles contain keyword 'democracy', 1990-2014
        Liberal government = Kim DJ 1998.02.25~2003.02.25, Roh MH 2003.02.25~2008.02.25
        Liberals dissatisfied = Leftist partisan dissatisfaction with democracy (source: CSES, 2004 wave)
      ") +
  scale_x_continuous(breaks = c(19980000, 20040000, 20080000), 
                     labels = c(1998, 2004, 2008)) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  annotate("rect", 
           xmin = 19980000, xmax = 20080000,
           ymin = 0, ymax = .20, 
           alpha = .2, fill = "grey") +
  annotate("rect", 
           xmin = 20040000, xmax = 20050000,
           ymin = 0, ymax = .20, 
           alpha = .5, fill = "yellow") +
  # annotate("rect", 
  #          xmin = 20070000, xmax = 20080000,
  #          ymin = 0, ymax = .20, 
  #          alpha = .3, fill = "green") +
  annotate("text", x = 19990000, label = "Liberal government", 
           y = 0.14, size = 3, angle = 90) +
  annotate("text", x = 20040000, label = "Liberals dissatisfied", 
           y = 0.14, size = 3, angle = 90) +
  # annotate("text", x = 20070000, label = "FTA negotiations", 
  #          y = 0.15, size = 3, angle = 90) +
  scale_fill_discrete(name = "Newspapers",
                      labels = labels) +
  scale_color_discrete(name = "Newspapers",
                       labels = labels) +
  facet_grid(.~topic, labeller = labeller(topic = topic_labels))
ggsave("plots/stm_int_effect_assoc.pdf", width = 6.5, height = 4)

findThoughts(topic_model, texts = covariates$Body, 
             n = 10, topics = 10)




