####################
### Wood & Grose ###
####################

# Preliminary note: to keep the object list from getting annoyingly long, 
# we re-use object names in generating confidence intervals and figures 
# generated from resampling. 
# So just be sure to re-run the entire section of code
# if you need to re-generate confidence intervals and figures.  

install.packages("readstata13")
install.packages("estimatr")
install.packages("stargazer")
install.packages("AER")
install.packages("MASS")
install.packages("ggplot2")
install.packages("sf")
install.packages("ggmap")
install.packages("plyr")
install.packages("dplyr")
install.packages("stringr")
install.packages("gridExtra")
install.packages("grid")
install.packages("sandwich")
install.packages("effects")
install.packages("modelsummary")

library(readstata13)
library(estimatr)
library(stargazer)
library(AER)
library(MASS)
library(ggplot2)
library(sf)
library(ggmap)
library(plyr)
library(dplyr)
library(stringr)
library(gridExtra)
library(grid)
library(sandwich)
library(effects)
library(modelsummary)

#set working directory (next line is an example of how to do that, but use your own folder names, obviously)

load("wood_grose_main_data_final.RData")

# General reporting note, which we remind you of before each table output:
# we report one-tailed p values throughout, except as discussed in table notes

#### TABLE 1 ####
retirement <- subset(maindata, year == 1978 &
                       !(maindata$name %in% c("LIVINGSTON", "GREEN", "GARCIA")))


mod1_retire <- lm_robust(
  retire__or_resign ~ audited * south,
  retirement,
  clusters = retirement$stcd)


mod2_retire <-
  lm_robust(
    retire__or_resign ~ audited * south + dvpinc2p_corrected + seniority,
    retirement,
    clusters = retirement$stcd)

mod3_retire <- lm_robust(retire__or_resign ~ audited,
                         retirement,
                         clusters = retirement$stcd)

# Table 1 output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod1_retire, mod2_retire, mod3_retire))

# f-stats visible using summary command:
summary(mod1_retire)
summary(mod2_retire)
summary(mod3_retire)


#### TABLE 2 ####
mod4_retire <- iv_robust(
  retire__or_resign ~ findings * south |
    audited * south,
  retirement,
  clusters = retirement$stcd)


mod5_retire <-
  iv_robust(
    retire__or_resign ~ findings * south + dvpinc2p_corrected + seniority |
      audited * south + dvpinc2p_corrected + seniority,
    retirement,
    clusters = retirement$stcd)


mod6_retire <- iv_robust(
  retire__or_resign ~ findings |
    audited,
  retirement,
  clusters = retirement$stcd)

# Table 2 output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod4_retire, mod5_retire, mod6_retire))

# f-stats visible using summary command:
summary(mod4_retire)
summary(mod5_retire)
summary(mod6_retire)



#### TABLE 3 ####
primary <- subset(maindata, year == 1978 &
                    incumbent_primary == 1 &
                    !(name %in% c("TONRY", "KOCH", "BADILLO")))

mod1_pcomp <- lm_robust(pcomp ~ audited * south,
                        primary,
                        clusters = primary$stcd)


mod2_pcomp <- lm_robust(
  pcomp ~ audited * south + dvpinc2p_corrected,
  primary,
  clusters = primary$stcd)


mod3_pcomp <- lm_robust(pcomp ~ audited,
                        primary,
                        clusters = primary$stcd)


mod4_prim <- lm_robust(
  numbercandidatesinprimary ~ audited * south,
  primary,
  clusters = primary$stcd)


mod5_prim <-
  lm_robust(
    numbercandidatesinprimary ~ audited * south + dvpinc2p_corrected,
    primary,
    clusters = primary$stcd)


mod6_prim <- lm_robust(
  numbercandidatesinprimary ~ audited,
  primary,
  clusters = primary$stcd)

# Table 3 output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod1_pcomp, mod2_pcomp, mod3_pcomp, mod4_prim, mod5_prim, mod6_prim))

# f-stats visible using summary command:
summary(mod1_pcomp)
summary(mod2_pcomp)
summary(mod3_pcomp)
summary(mod4_prim)
summary(mod5_prim)
summary(mod6_prim)

#### TABLE 4 ####
mod7_pcomp <- iv_robust(
  pcomp ~ findings * south |
    audited * south,
  primary,
  clusters = primary$stcd)


mod8_pcomp <-
  iv_robust(
    pcomp ~ findings * south + dvpinc2p_corrected |
      audited * south + dvpinc2p_corrected,
    primary,
    clusters = primary$stcd)



mod9_pcomp <- iv_robust(pcomp ~ findings |
                          audited,
                        primary,
                        clusters = primary$stcd)


mod10_prim <- iv_robust(
  numbercandidatesinprimary ~ findings * south |
    audited * south,
  primary,
  clusters = primary$stcd)


mod11_prim <-
  iv_robust(
    numbercandidatesinprimary ~ findings * south + dvpinc2p_corrected |
      audited * south + dvpinc2p_corrected,
    primary,
    clusters = primary$stcd)


mod12_prim <- iv_robust(
  numbercandidatesinprimary ~ findings |
    audited,
  primary,
  clusters = primary$stcd)

# Table 4 output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod7_pcomp, mod8_pcomp, mod9_pcomp, mod10_prim, mod11_prim, mod12_prim))

# f-stats visible using summary command:
summary(mod7_pcomp)
summary(mod8_pcomp)
summary(mod9_pcomp)
summary(mod10_prim)
summary(mod11_prim)
summary(mod12_prim)



#### TABLE 5 ####
gen <- subset(maindata, year == 1978 & incumbent == 1 & 
                !(name %in% c("TONRY", "KOCH", "BADILLO")))

mod1_gen <- lm_robust(dvinc2p ~ audited * south,
                      gen,
                      clusters = gen$stcd)


mod2_gen <- lm_robust(
  dvinc2p ~ audited * south + dvpinc2p_corrected,
  gen,
  clusters = gen$stcd)


mod3_gen <- lm_robust(dvinc2p ~ audited,
                      gen,
                      clusters = gen$stcd)

# Table 5 output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod1_gen, mod2_gen, mod3_gen))

# f-stats visible using summary command:
summary(mod1_gen)
summary(mod2_gen)
summary(mod3_gen)


##### TABLE 6 ####
mod4_gen <- iv_robust(
  dvinc2p ~ findings * south |
    audited * south,
  gen,
  clusters = gen$stcd)


mod5_gen <-
  iv_robust(
    dvinc2p ~ findings * south + dvpinc2p_corrected |
      audited * south + dvpinc2p_corrected,
    gen,
    clusters = gen$stcd)


mod6_gen <- iv_robust(dvinc2p ~ findings |
                        audited,
                      gen,
                      clusters = gen$stcd)
# Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod4_gen, mod5_gen, mod6_gen))

# f-stats visible using summary command:
summary(mod4_gen)
summary(mod5_gen)
summary(mod6_gen)


#### TABLE 7 ####
# Note that this table can be replicated using the STATA .do file 
# called Table 7 AJPS Wood Grose House Incumbent Vote Share 2006 to 2018 regression commands & codebook.do
# and the data file called Table 7 AJPS Wood Grose House Incumbent Vote Share 2006 to 2018.dta 


#### TABLE 8 ####
# Note that this table can be replicated using the STATA .do file 
# called Table 8 AJPS Wood Grose News Coverage of House Members regression commands and codebook.do,
# and the data files called Lexis Local News Advance 2018 Data House media CF scandal.dta and 
# Table 8 Wood Grose News Coverage 2010 & 2014 Data.dta


#### CCES mentions in main text and Appendix L ####
## CCES analysis is in Appendix L.do
## and that .do file uses this data file:  "CCES_cumulative_2006_2018_with_scandals.dta"


#### Ads ####
## Note that we include the code for replicating our analysis of the advertising, but future scholars will have to 
## ask Wesleyan Media Project for the data and sign a data release with them.  

## we use the following variables from WMP:  
## issue91: Government Ethics/Scandal
## election
## tgt_id, category, and cand_id (cleaned) to identify whether the incumbent is the target (our variable is inc_targeted)
## tonecmag as a dummy variable when the ad is negative (our "neg")
## airdate, state, district for merging
## and we merge with our scandal data for a dummy variable to identify two things 
## (1) when the district has a campaign finance scandal (cf_scandal)
## (2) who the incumbent is (since it's not identified in the WMD data.)

# download the following Wesleyan Media Project data (in Stata format):
# directions are in the README, but here's their data access page -- 
# it's self-explanatory: https://mediaproject.wesleyan.edu/dataaccess/
# here's the data and versions to get to replicate our findings:
# 2012 U.S. House v1.1 
# 2014 U.S. House v.1.0
# 2016 U.S. House v.1.0

# cleaning and analysis code follows, but is commented out so it doesn't break if you run the whole file.

# load data from working directory
# w12 <- read.dta13("wmp-house-2012-v1.1.dta", generate.factors=T)
# w14 <- read.dta13("wmp-house-2014-v1.0.dta", generate.factors=T)
# w16 <- read.dta13("wmp-house-2016-v1.0.dta", generate.factors=T)
# 
#   # save variables we need for analysis
# w12new <- subset(w12, select = c(airdate, election, tonecmag, district,  
#                 tgt_id, cnt_prp, issue91, state, cand_id))
# w14new <- subset(w14, select = c(airdate, election, tonecmag, district,  
#                                  tgt_id, cnt_prp, issue91, state, cand_id))
# w16new <- subset(w16, select = c(airdate, election, tonecmag, district,  
#                                  tgt_id, cnt_prp, issue91, state, cand_id))
# # 2016 ad data doesn't have a 1/0 on issue91 (Gov ethics / scandal)
# # but instead is either "Government Ethics / Scandal", "0", or "NA"
# w16new$issue91 <- as.character(w16new$issue91)
# w16new$issue91[w16new$issue91 == "Government Ethics / Scandal"] <- 1
# w16new$issue91 <- as.numeric(w16new$issue91)
# 
# # 2016 ad data uses words for cnt_prp whereas 2012 & 2014 use numbers.  
# # Changing to numbers to conform with 2012 and 2014
# w16new$cnt_prp <- as.character(w16new$cnt_prp)
# w16new$cnt_prp[w16new$cnt_prp == "More Promote Than Attack"] <- 1
# w16new$cnt_prp[w16new$cnt_prp == "About Equal Attack and Promote"] <- 2
# w16new$cnt_prp[w16new$cnt_prp == "More Attack Than Promote"] <- 3
# w16new$cnt_prp[w16new$cnt_prp == "Only Contrasting Element is Brief Statement in Oral Authorization"] <- 4
# w16new$cnt_prp <- as.numeric(w16new$cnt_prp)
# 
# 
# data <- rbind(w12new, w14new, w16new) 
#                  
# data$district[data$district == "1"] <- "01"
# data$district[data$district == "2"] <- "02"
# data$district[data$district == "3"] <- "03"
# data$district[data$district == "4"] <- "04"
# data$district[data$district == "5"] <- "05"
# data$district[data$district == "6"] <- "06"
# data$district[data$district == "7"] <- "07"
# data$district[data$district == "8"] <- "08"
# data$district[data$district == "9"] <- "09"
# data$district[data$district == "N/A"] <- "NA"
# data$district[data$district == ""] <- "NA"
#                  
# data$stcd <- paste(data$state, "-", data$district, sep = "")
#                  
# year <- substr(data$airdate, 1, 4)
# data$year <- year
# 
# # we saved this data to have a clean set to return to.  
# # Including this line here, though it's not strictly needed.
# #save(data, file = "ad_data_for_replication.RData")
# 
# # load scandal data
# c1 <- read.csv("scandaldata_rep.csv")
# 
# # now put the scandals in the Wesleyan data ####
# c1$cdchar <- as.character(c1$cd)
# 
# ads <- merge(c1, data, by.x = c("year", "cdchar"), 
#              by.y = c("year", "stcd"), all.x = F, all.y = T)
# 
# 
# # now need to figure out when incumbents are involved in these ads.
# # step 1: get last names in a comparable format
# # get the last name in tgt_id, which is Lastname_Firstname
#   y <- strsplit(ads$tgt_id, "_")
#                   
#   tgt_last = rep(NA, length(y))
#   for (i in 1:length(y)){
#     tgt_last[i] <- y[[i]][1]
#   }
#                   
#   ads$tgt_last <- tgt_last
#                 
#               
# # now get last name of current_rep
#  ads$rep_current <- as.character(ads$rep_current)
#                 
#   z <- strsplit(ads$rep_current, " ")
#                 
#   inc_last = rep(NA, length(z))
#   for (i in 1:length(z)){
#     inc_last[i] <- z[[i]][2]
#     }
#                 
#     ads$inc_lastname <- inc_last
#                 
#                 
# # step 2:  1/0 for whether incumbent targeted 
#   ads$inc_targeted <- as.numeric(ads$inc_lastname == ads$tgt_last)
#                 
#  
# # clean a few more things (election type, ad tone, ad tone x target)               
#   ads$election[ads$election == "PRIMARY - DEMOCRATIC"] <- "PRIMARY"
#   ads$election[ads$election == "PRIMARY - REPUBLICAN"] <- "PRIMARY"
#   ads$election[ads$election == "RUNOFF"] <- "GENERAL"
#   ads$election[ads$election == "SPECIAL"] <- "GENERAL"
#   
#   ads$general <- as.numeric(ads$election == "GENERAL")
#   
#   ads$neg <- as.numeric((ads$tonecmag == "CONTRAST" & ads$cnt_prp == 3) | 
#                                ads$tonecmag == "NEG" |
#                                ads$tonecmag == "NEGATIVE")
#   
#   ads$negvsinc <- as.numeric(ads$neg == 1 & ads$inc_targeted == 1)
#   
#   # use this for second t-test where it's negative, it mentions gov ethics or scandal, and
#   # we have coded it as a campaign finance scandal on govtrack.
#   ads$negscandal <- as.numeric(ads$neg == 1 & ads$issue91 == 1)
#   
# 
# ## here's the code to use to replicate the advertising t.tests, 
# 
# # Ads mentioning government scandals were more negative (84%) 
# # than ads not mentioning government scandals (50%, p=0.00)
# t.test(ads$neg[ads$issue91 == 1], ads$neg[ads$issue91 == 0])
# 
# # such ads were more likely in districts with campaign finance scandals (7.8%) 
# # than in districts without campaign finance scandals (6.7%, p=0.00). 
# t.test(ads$negscandal[ads$cfscandal == 1], ads$negscandal[ads$cfscandal == 0])  
# 
# #Among ads that mention scandals and target incumbents, 100% of such ads were 
# # negative over the time period in districts that experienced campaign finance scandals, 
# # compared to districts that did not (85%, p=0.00), 
# diff3 <- t.test(ads$neg[ads$issue91 == 1 & ads$cfscandal == 1 & ads$inc_targeted == 1], 
#                 ads$neg[ads$issue91 == 1 & ads$cfscandal == 0 & ads$inc_targeted == 1])
# 
# # Negative ads are more prominent in primary elections with campaign finance scandals (78%) 
# # compared to those without (21%, p=0.00). 
# diff4 <- t.test(ads$neg[ads$cfscandal == 1 & ads$election == "PRIMARY"], 
#                 ads$neg[ads$cfscandal == 0 & ads$election == "PRIMARY"])  
# 





#### FIGURE 1 ####
get_congress_map <- function(cong) {
  tmp_file <- tempfile()
  tmp_dir  <- tempdir()
  zp <-
    sprintf("http://cdmaps.polisci.ucla.edu/shp/districts%03i.zip",
            cong)
  download.file(zp, tmp_file)
  unzip(zipfile = tmp_file, exdir = tmp_dir)
  fpath <-
    paste(tmp_dir,
          sprintf("districtShapes/districts%03i.shp", cong),
          sep = "/")
  st_read(fpath)
}
mapdata <- maindata
# grab the congressional district from the stcd variable
mapdata$cd <- str_sub(mapdata$stcd, start = -2)

cong.map <- get_congress_map(94)
names(cong.map)[c(1, 3)] <- c("statename", "cd")
mapdata$audit_map <- case_when(
  mapdata$audited == 0 ~ "Control",
  mapdata$audited == 1 & mapdata$findings == 0 ~ "Clean audit",
  mapdata$audited == 1 & mapdata$findings == 1 ~ "Dirty audit",
)
mapdata$cd <- str_pad(mapdata$cd, pad = "0", width = 2)
cong.map$cd <- str_pad(cong.map$cd, pad = "0", width = 2)
cong.map$cd[cong.map$cd == "00"] <- "01"
cong.map$statename <- toupper(substr(cong.map$statename, 1, 7))

cong.map <- left_join(cong.map, mapdata[mapdata$year == 1978,
                                        c("audit_map", "statename", "cd")],
                      match = "first")

cong.map$audit_map <- factor(cong.map$audit_map,
                             levels = c("Dirty audit", "Clean audit", "Control"))

pdf("figure1.pdf", width = 10, height = 6)
ggplot() +
  geom_sf(data = cong.map[!(cong.map$statename %in% c("HAWAII", "ALASKA")), ],
          aes(fill = audit_map)) +
  theme_void() + theme(legend.position = "bottom") +
  scale_fill_manual(name = "", values = c("grey10", "Grey", "White")) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
dev.off()


#### FIGURE 2 ####
disclose_num <-
  ggplot(maindata[maindata$year == 1976 &
                   maindata$incumbent == 1 & maindata$audited == 1 &
                   maindata$findings == 1, ]) + theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(aes(x = disclosurenum), bins = 20) + theme_classic() +
  xlab("Number of Violations") + ylab("Frequency") +
  ggtitle("Disclosure Violations") + theme(text = element_text(size = 7))

misstate_num <-
  ggplot(maindata[maindata$year == 1976 &
                   maindata$incumbent == 1 & maindata$audited == 1 &
                   maindata$findings == 1, ]) + theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(aes(x = misstatenum), bins = 20) + theme_classic() +
  xlab("Number of Violations") + ylab("Frequency") +
  ggtitle("Misstatements") + theme(text = element_text(size = 7))

records_num <-
  ggplot(maindata[maindata$year == 1976 &
                   maindata$incumbent == 1 & maindata$audited == 1 &
                   maindata$findings == 1, ]) + theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(aes(x = recordnum), bins = 20) + theme_classic() +
  xlab("Number of Violations") + ylab("Frequency") +
  ggtitle("Records Violations") + theme(text = element_text(size = 7))

excess_num <-
  ggplot(maindata[maindata$year == 1976 &
                   maindata$incumbent == 1 & maindata$audited == 1 &
                   maindata$findings == 1, ]) + theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(aes(x = excesscontribnum), bins = 20) + theme_classic() +
  xlab("Number of Violations") + ylab("Frequency") +
  ggtitle("Excess Contributions") + theme(text = element_text(size = 7))

prohib_num <-
  ggplot(maindata[maindata$year == 1976 &
                   maindata$incumbent == 1 & maindata$audited == 1 &
                   maindata$findings == 1, ]) + theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(aes(x = prohibcontribnum), bins = 20) + theme_classic() +
  xlab("Number of Violations") + ylab("Frequency") +
  ggtitle("Prohibited Sources") + theme(text = element_text(size = 7))

total_num <-
  ggplot(maindata[maindata$year == 1976 &
                   maindata$incumbent == 1 & maindata$audited == 1 &
                   maindata$findings == 1, ]) + theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(aes(x = totalviolations), bins = 20) + theme_classic() +
  xlab("Number of Violations") + ylab("Frequency") +
  ggtitle("Total Violations") + theme(text = element_text(size = 7))

disclose_dollar <-
  ggplot(maindata[maindata$year == 1976 &
                   maindata$incumbent == 1 & maindata$audited == 1 &
                   maindata$findings == 1, ]) +
  geom_histogram(aes(x = disclosuredollar), bins = 20) + theme_classic() +
  xlab("Dollars") + ylab("Frequency") + theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Disclosure Violations") + theme(text = element_text(size = 7))

misstate_dollar <-
  ggplot(maindata[maindata$year == 1976 &
                   maindata$incumbent == 1 & maindata$audited == 1 &
                   maindata$findings == 1, ]) +
  geom_histogram(aes(x = misstatedol), bins = 20) + theme_classic() +
  xlab("Dollars") + ylab("Frequency") + theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Misstatements") + theme(text = element_text(size = 7))

records_dollar <-
  ggplot(maindata[maindata$year == 1976 &
                   maindata$incumbent == 1 & maindata$audited == 1 &
                   maindata$findings == 1, ]) +
  geom_histogram(aes(x = recorddollar), bins = 20) + theme_classic() +
  xlab("Dollars") + ylab("Frequency") + theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Records Violations") + theme(text = element_text(size = 7))

excess_dollar <-
  ggplot(maindata[maindata$year == 1976 &
                   maindata$incumbent == 1 & maindata$audited == 1 &
                   maindata$findings == 1, ]) +
  geom_histogram(aes(x = excesscontribdollar), bins = 20) + theme_classic() +
  xlab("Dollars") + ylab("Frequency") + theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Excess Contributions") + theme(text = element_text(size = 7))

prohib_dollar <-
  ggplot(maindata[maindata$year == 1976 &
                   maindata$incumbent == 1 & maindata$audited == 1 &
                   maindata$findings == 1, ]) +
  geom_histogram(aes(x = prohibcontribdollar), bins = 20) + theme_classic() +
  xlab("Dollars") + ylab("Frequency") + theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Prohibited Sources") + theme(text = element_text(size = 7))

total_dollar <-
  ggplot(maindata[maindata$year == 1976 &
                   maindata$incumbent == 1 & maindata$audited == 1 &
                   maindata$findings == 1, ]) +
  geom_histogram(aes(x = totdollarviolations), bins = 20) + theme_classic() +
  xlab("Dollars") + ylab("Frequency") + theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Total Violations") + theme(text = element_text(size = 7))


g2 <- grid.arrange(
  disclose_dollar,
  misstate_dollar,
  records_dollar,
  excess_dollar,
  prohib_dollar,
  total_dollar,
  nrow = 2
)

pdf(file = "violations_num.pdf",
    width = 9 / 2,
    height = 3)
grid.arrange(disclose_num,
             misstate_num,
             records_num,
             excess_num,
             prohib_num,
             total_num,
             nrow = 2)
grid.rect(width = .99,
          height = .99,
          gp = gpar(lwd = 1, fill = NA))
dev.off()

pdf(file = "violations_dollar.pdf",
    width = 9 / 2,
    height = 3)
grid.arrange(
  disclose_dollar,
  misstate_dollar,
  records_dollar,
  excess_dollar,
  prohib_dollar,
  total_dollar,
  nrow = 2
)
grid.rect(width = .99,
          height = .99,
          gp = gpar(lwd = 1, fill = NA))
dev.off()


#### APPENDIX ####

#### Appendix A ####
stargazer(retirement[retirement$totalviolations>0,
                  c("disclosurenum", "disclosuredollar",
                    "epcontribnum", "epcontribdollar",
                    "recordnum", "recorddollar",
                    "misstatenum", "misstatedol",
                    "totalviolations", "totdollarviolations")],
          type="text", summary.stat = c("min","max","mean","sd","n"))

#### Appendix B ####
mod_b1 <- lm_robust(totalaudithits_all ~ findings + totalhits, 
                  data = retirement[retirement$name!="TONRY" &
                                      retirement$name!="KOCH" & 
                                      retirement$name!="BADILLO",])
mod_b2 <- lm_robust(totalaudithits_all ~ findings10k + totalhits, 
                    data = retirement[retirement$name!="TONRY" &
                                        retirement$name!="KOCH" & 
                                        retirement$name!="BADILLO",])
# Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod_b1, mod_b2))

# f-stats visible using summary command:
summary(mod1_b1)
summary(mod2_b2)


#### Appendix C ####
ttest_c1 <- t.test(retirement$dvpinc2p_corrected[retirement$audited==0],
                   retirement$dvpinc2p_corrected[retirement$audited==1])
ttest_c2 <- t.test(retirement$south[retirement$audited==0],
                   retirement$south[retirement$audited==1])
ttest_c3 <- t.test(retirement$comm_chair[retirement$audited==0],
                   retirement$comm_chair[retirement$audited==1])
ttest_c4 <- t.test(retirement$leaders[retirement$audited==0],
                   retirement$leaders[retirement$audited==1])
ttest_c5 <- t.test(retirement$seniority[retirement$audited==0],
                   retirement$seniority[retirement$audited==1])
ttest_c6 <- t.test(retirement$ageinyears1976[retirement$audited==0],
                   retirement$ageinyears1976[retirement$audited==1])
ttest_c7 <- t.test(retirement$distancetodist[retirement$audited==0]/100,
                   retirement$distancetodist[retirement$audited==1]/100)

# data-frame to replicate Table in Appendix C
variable <- c("Dist_pres_vote", "South", "Committee_chair", "Party_leader", 
              "Seniority", "Age", "Distance_to_DC")
mean_control <- c(ttest_c1$estimate[[1]], ttest_c2$estimate[[1]],  ttest_c3$estimate[[1]],
                  ttest_c4$estimate[[1]],  ttest_c5$estimate[[1]],  ttest_c6$estimate[[1]], 
                  ttest_c7$estimate[[1]])
mean_treatment <- c(ttest_c1$estimate[[2]], ttest_c2$estimate[[2]],  ttest_c3$estimate[[2]],
                  ttest_c4$estimate[[2]],  ttest_c5$estimate[[2]],  ttest_c6$estimate[[2]], 
                  ttest_c7$estimate[[2]])
p.value <- c(ttest_c1$p.value, ttest_c2$p.value,  ttest_c3$p.value,
             ttest_c4$p.value,  ttest_c5$p.value,  ttest_c6$p.value, 
             ttest_c7$p.value)

results_table_to_round <- cbind(mean_control, mean_treatment, p.value)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

results_tab_rounded <- round_df(results_table_to_round, 2)
cbind(variable, results_tab_rounded)


#### Appendix D ####
#model 1
mod1_retire_logit <- glm(
  retire__or_resign ~ audited * south,
  retirement,
  family="binomial",
)

# robust ses
mod1_retire_logit_se <- coeftest(mod1_retire_logit, vcov = vcovHC(mod1_retire_logit, type = "HC1"))

# model 2
mod2_retire_logit <- glm(
  retire__or_resign ~ audited * south + dvpinc2p_corrected + seniority,
  retirement,
  family="binomial",
)
# robust ses
mod2_retire_logit_se <- coeftest(mod2_retire_logit, vcov = vcovHC(mod2_retire_logit, type = "HC1"))


# model 3
mod3_retire_logit <- glm(
  retire__or_resign ~ audited,
  retirement,
  family="binomial",
)
#robust ses
mod3_retire_logit_se <- coeftest(mod3_retire_logit, vcov = vcovHC(mod3_retire_logit, type = "HC1"))

#Table output, at least for estimates, N, etc., but please use robust ses 
# mod1_retire_logit_se, mod2_retire_logit_se, mod3_retire_logit_se
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod1_retire_logit, mod2_retire_logit, mod3_retire_logit))

# f-stats visible using summary command:
summary(mod1_retire_logit)
summary(mod2_retire_logit)
summary(mod3_retire_logit)

#### Appendix E ####
primary$close100 <- ifelse(primary$distancetodist<=100,1,0)
tripsdata <- subset(primary, complete.cases(numbertrips_actually_taken1q1976))


# model 1 
# use "primary" data because no lagged trips on RHS
mod1_trip_nb <- glm.nb(numbertrips_actually_taken1q1978  ~ audited * south,
                       primary)
# robust SEs, model 2
mod1_trip_nb_se <- coeftest(mod1_trip_nb, vcov = vcovHC(mod1_trip_nb, type = "HC1"))


# predicted values 
# (This function treats the indicator variables as continuous, 
# but you just need to get the predicted values 
# from the four corners of the effect table)
summary(allEffects(mod1_trip_nb))


# model 2
# use tripsdata because lagged trips here
mod2_trip_nb <- glm.nb(numbertrips_actually_taken1q1978  ~ audited * south +
                         dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
                         I(distancetodist/100) +close100,
                       tripsdata)
# robust SEs, model 2
mod2_trip_nb_se <- coeftest(mod2_trip_nb, vcov = vcovHC(mod2_trip_nb, type = "HC1"))


# predicted values
# (again, this spits out a ton of stuff -- just look for the effect matrix
# for audited*south effect)
summary(allEffects(mod2_trip_nb))


# model 3
# use primary data here
mod3_trip_nb <- glm.nb(numbertrips_actually_taken1q1978  ~ audited,
                       primary)

# robust SEs, model 3
mod3_trip_nb_se <- coeftest(mod3_trip_nb, vcov = vcovHC(mod3_trip_nb, type = "HC1"))

# predicted values
summary(allEffects(mod3_trip_nb))

#Table output, at least for estimates, N, etc., but please use robust ses 
# mod1_trip_nb_se, mod2_trip_nb_se, mod3_trip_nb_se
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod1_trip_nb, mod2_trip_nb, mod3_trip_nb))

# additional fit information available using summary():
summary(mod1_trip_nb)
summary(mod2_trip_nb)
summary(mod3_trip_nb)

#### Appendix F ####
# note that there's no code to save any of this as separate objects or as documents
# on your machine; if you want to do that, you'll need to give the objects unique names, 
# because we are overwriting them here.

#### Figure F1 ####
tmp <- retirement[retirement$south==0,]
N <- 10000
est.boot1 <- as.data.frame(matrix(NA, nrow=N, ncol=4))
colnames(est.boot1) <- c('intercept', 'audited.nonsouth', 'audited.south', 
                         'south')

N <- 10000 # number of times to loop
set.seed(555)

for(i in 1:N){
  
  # generate data and reassign treatment
  sample <- sample(c(TRUE, FALSE), nrow(tmp), replace = T, prob = c(0.1,0.9))
  tr <- tmp[sample, ]
  tr$tr <- 1
  ct <- tmp[!sample, ]
  ct$tr <- 0
  sample1 <- rbind(tr, ct)
  
  # regression
  lm_ret_d_c.boot <- lm_robust(retire__or_resign ~ tr + I(tr * south) + 
                                 south, data = sample1)
  
  ## Save coefficients 
  est.boot1[i,] <- c(lm_ret_d_c.boot$coef[1:4])
  
  #print(i) # so you can watch the progress of the loop
}



est.plot1 <- data.frame(x = density(est.boot1$audited.nonsouth)[[1]],
                               y = density(est.boot1$audited.nonsouth)[[2]])

p5 <- quantile(est.boot1$audited.nonsouth, .025)
p95 <- quantile(est.boot1$audited.nonsouth, .975)

est.plot1$on <- ifelse(est.plot1$x > p5 &
                         est.plot1$x < p95, 1, 0)



ggplot(est.plot1,
       aes(x=x, y=y)) +
  geom_line() + 
  geom_area(data = filter(est.plot1, on == 1), fill = 'grey') +
  theme_classic() + geom_vline(xintercept = coef(mod1_retire)[2], col = "red") +
  xlab("") + ylab("Density") + theme(panel.grid = element_blank())



#### Figure F2 ####
# sampling from primary dataset becuase not controlling for trips taken in 1976
tmp <- primary[primary$south==0,]
N <- 10000
est.boot1 <- as.data.frame(matrix(NA, nrow=N, ncol=4))
colnames(est.boot1) <- c('intercept', 'audited.nonsouth', 'audited.south', 
                         'south')

N <- 10000 # number of times to loop
set.seed(555)

for(i in 1:N){
  
  # generate data and reassign treatment
  sample <- sample(c(TRUE, FALSE), nrow(tmp), replace = T, prob = c(0.1,0.9))
  tr <- tmp[sample, ]
  tr$tr <- 1
  ct <- tmp[!sample, ]
  ct$tr <- 0
  sample1 <- rbind(tr, ct)
  
  # regression
  lm_ret_d_c.boot <- lm_robust(numbertrips_actually_taken1q1978 ~ tr + I(tr * south) + 
                                 south, data = sample1)
  
  ## Save coefficients 
  est.boot1[i,] <- c(lm_ret_d_c.boot$coef[1:4])
  
 # print(i) # so you can watch the progress of the loop
}



est.plot1 <- data.frame(x = density(est.boot1$audited.nonsouth)[[1]],
                        y = density(est.boot1$audited.nonsouth)[[2]])

p5 <- quantile(est.boot1$audited.nonsouth, .025)
p95 <- quantile(est.boot1$audited.nonsouth, .975)

est.plot1$on <- ifelse(est.plot1$x > p5 &
                         est.plot1$x < p95, 1, 0)


ggplot(est.plot1,
       aes(x=x, y=y)) +
  geom_line() + 
  geom_area(data = filter(est.plot1, on == 1), fill = 'grey') +
  theme_classic() + geom_vline(xintercept = coef(mod1_trip)[2], col = "red") +
  xlab("") + ylab("Density") + theme(panel.grid = element_blank())


#### Figure F3 ####
tmp <- primary[primary$incumbent == 1 & 
                 primary$retire__or_resign == 0,]
N <- 10000
est.boot1 <- as.data.frame(matrix(NA, nrow=N, ncol=4))
colnames(est.boot1) <- c('intercept', 'audited.nonsouth', 'audited.south', 
                         'south')

N <- 10000 # number of times to loop
set.seed(555)

for(i in 1:N){
  
  # generate data and reassign treatment
  sample <- sample(c(TRUE, FALSE), nrow(tmp), replace = T, prob = c(0.1,0.9))
  tr <- tmp[sample, ]
  tr$tr <- 1
  ct <- tmp[!sample, ]
  ct$tr <- 0
  sample1 <- rbind(tr, ct)
  
  # regression
  lm_ret_d_c.boot <- lm_robust(pcomp ~ tr + I(tr * south) + 
                                 south, data = sample1)
  
  ## Save coefficients 
  est.boot1[i,] <- c(lm_ret_d_c.boot$coef[1:4])
  
 # print(i) # so you can watch the progress of the loop
}



est.plot1 <- data.frame(x = density(est.boot1$audited.south, na.rm = T)[[1]],
                        y = density(est.boot1$audited.south, na.rm = T)[[2]])

p5 <- quantile(est.boot1$audited.south, .025, na.rm = T)
p95 <- quantile(est.boot1$audited.south, .975, na.rm = T)

est.plot1$on <- ifelse(est.plot1$x > p5 &
                         est.plot1$x < p95, 1, 0)


est.boot2 <- as.data.frame(matrix(NA, nrow=N, ncol=4))
colnames(est.boot2) <- c('intercept', 'audited.nonsouth', 'audited.south', 
                         'south')

N <- 10000 # number of times to loop
set.seed(555)

for(i in 1:N){
  
  # generate data and reassign treatment
  sample <- sample(c(TRUE, FALSE), nrow(tmp), replace = T, prob = c(0.1,0.9))
  tr <- tmp[sample, ]
  tr$tr <- 1
  ct <- tmp[!sample, ]
  ct$tr <- 0
  sample1 <- rbind(tr, ct)
  
  # regression
  lm_ret_d_c.boot <- lm_robust(numbercandidatesinprimary ~ tr + I(tr * south) + 
                                 south, data = sample1)
  
  ## Save coefficients 
  est.boot2[i,] <- c(lm_ret_d_c.boot$coef[1:4])
  
#  print(i) # so you can watch the progress of the loop
}



est.plot2 <- data.frame(x = density(est.boot2$audited.south, na.rm = T)[[1]],
                        y = density(est.boot2$audited.south, na.rm = T)[[2]])

p5 <- quantile(est.boot2$audited.south, .025, na.rm = T)
p95 <- quantile(est.boot2$audited.south, .975, na.rm = T)

est.plot2$on <- ifelse(est.plot2$x > p5 &
                         est.plot2$x < p95, 1, 0)





ggplot(est.plot1,
       aes(x=x, y=y)) +
  geom_line() + 
  geom_area(data = filter(est.plot1, on == 1), fill = 'grey') +
  theme_classic() + geom_vline(xintercept = coef(mod1_pcomp)[4], col = "red") +
  xlab("") + ylab("Density") + theme(panel.grid = element_blank())

ggplot(est.plot2,
       aes(x=x, y=y)) +
  geom_line() + 
  geom_area(data = filter(est.plot2, on == 1), fill = 'grey') +
  theme_classic() + geom_vline(xintercept = coef(mod4_prim)[4], col = "red") +
  xlab("") + ylab("Density") + theme(panel.grid = element_blank())


#### Figure F4 ####
tmp <- retirement[retirement$south==0,]
N <- 10000
est.boot1 <- as.data.frame(matrix(NA, nrow=N, ncol=4))
colnames(est.boot1) <- c('intercept', 'audited.nonsouth', 'audited.south', 
                         'south')

N <- 10000 # number of times to loop
set.seed(555)

for(i in 1:N){
  
  # generate data and reassign treatment
  sample <- sample(c(TRUE, FALSE), nrow(tmp), replace = T, prob = c(0.1,0.9))
  tr <- tmp[sample, ]
  tr$tr <- 1
  ct <- tmp[!sample, ]
  ct$tr <- 0
  sample1 <- rbind(tr, ct)
  
  # regression
  lm_ret_d_c.boot <- lm_robust(dvinc2p ~ tr + I(tr * south) + 
                                 south, data = sample1)
  
  ## Save coefficients 
  est.boot1[i,] <- c(lm_ret_d_c.boot$coef[1:4])
  
#  print(i) # so you can watch the progress of the loop
}



est.plot1 <- data.frame(x = density(est.boot1$audited.nonsouth)[[1]],
                        y = density(est.boot1$audited.nonsouth)[[2]])

p5 <- quantile(est.boot1$audited.nonsouth, .025)
p95 <- quantile(est.boot1$audited.nonsouth, .975)

est.plot1$on <- ifelse(est.plot1$x > p5 &
                         est.plot1$x < p95, 1, 0)



ggplot(est.plot1,
       aes(x=x, y=y)) +
  geom_line() + 
  geom_area(data = filter(est.plot1, on == 1), fill = 'grey') +
  theme_classic() + geom_vline(xintercept = coef(mod1_gen)[2], col = "red") +
  xlab("") + ylab("Density") + theme(panel.grid = element_blank())


#### Appendix G ####
#### Table G1 
mod1_retire_full <-
  lm_robust(
    retire__or_resign ~ audited * south + dvpinc2p_corrected + seniority +
      comm_chair + leaders + scandal78,
    retirement,
    clusters = retirement$stcd)


mod2_retire_full <-
  lm_robust(
    retire__or_resign ~ audited * south + dvpinc2p_corrected + ageinyears1976 +
      comm_chair + leaders + scandal78,
    retirement,
    clusters = retirement$stcd)

# Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod1_retire_full, mod2_retire_full))

# f-stats visible using summary command:
summary(mod1_retire_full)
summary(mod2_retire_full)


#### Table G2 ####
mod3_retire_full <- iv_robust(retire__or_resign  ~ findings*south 
          + dvpinc2p_corrected + seniority + comm_chair + leaders + scandal78| 
            audited*south + dvpinc2p_corrected + seniority +
            leaders + scandal78 + comm_chair,  
          data = retirement, clusters = retirement$stcd)

mod4_retire_full <- iv_robust(retire__or_resign  ~ findings*south 
                    + dvpinc2p_corrected + ageinyears1976 + comm_chair + leaders + scandal78| 
                    audited*south + dvpinc2p_corrected + ageinyears1976 +
                    leaders + scandal78 + comm_chair,  
                    data = retirement, clusters = retirement$stcd)

# Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod3_retire_full, mod4_retire_full))

# f-stats visible using summary command:
summary(mod3_retire_full)
summary(mod4_retire_full)


#### Table G3 ####
mod1_trip_full <-
  lm_robust(
    numbertrips_actually_taken1q1978  ~ audited * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100 + qualchvsinc + oppo_minusinc_exp + scandal78,
    tripsdata,
    clusters = tripsdata$stcd)

# Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod1_trip_full))

# f-stats visible using summary command:
summary(mod1_trip_full)

#### Table G4 ####
mod2_trip_full <-
  iv_robust(
    numbertrips_actually_taken1q1978  ~ findings * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100 + qualchvsinc + oppo_minusinc_exp + scandal78|
      audited * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100 + qualchvsinc + oppo_minusinc_exp + scandal78,
    tripsdata,
    clusters = tripsdata$stcd)

# Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod2_trip_full))

# f-stats visible using summary command:
summary(mod2_trip_full)


#### Table G5 ####
mod1_pcomp_full <- lm_robust(pcomp ~ audited * south +
                               dvpinc2p_corrected + scandal78,
                      primary, clusters = primary$stcd)


mod1_prim_full <- lm_robust(numbercandidatesinprimary ~ audited * south +
                               dvpinc2p_corrected + scandal78,
                             primary, clusters = primary$stcd)

# Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod1_pcomp_full, mod1_prim_full))

# f-stats visible using summary command:
summary(mod1_pcomp_full)
summary(mod1_prim_full)


#### Table G6 ####
mod2_pcomp_full <- iv_robust(pcomp ~ findings * south +
                               dvpinc2p_corrected + scandal78|
                               audited * south +
                               dvpinc2p_corrected + scandal78,
                             primary, clusters = primary$stcd)

mod2_prim_full <- iv_robust(numbercandidatesinprimary ~ findings * south +
                              dvpinc2p_corrected + scandal78|
                              audited * south +
                              dvpinc2p_corrected + scandal78,
                            primary, clusters = primary$stcd)

# Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod2_pcomp_full, mod2_prim_full))

# f-stats visible using summary command:
summary(mod2_pcomp_full)
summary(mod2_prim_full)


#### Table G7 ####
mod1_gen_full <- lm_robust(dvinc2p ~ audited * south + dvpinc2p_corrected +
                             qualchvsinc + oppo_minusinc_exp + unoppo +
                             scandal78,
                           gen,
                           clusters = gen$stcd)

#Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(mod1_gen_full)

# f-stats visible using summary command:
summary(mod1_gen_full)


#### Table G8 ####
mod2_gen_full <- iv_robust(dvinc2p ~ findings * south + dvpinc2p_corrected +
                             qualchvsinc + oppo_minusinc_exp + unoppo +
                             scandal78 | audited * south + dvpinc2p_corrected +
                             qualchvsinc + oppo_minusinc_exp + unoppo +
                             scandal78,
                           gen, clusters = gen$stcd)

#Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod2_gen_full))

# f-stats visible using summary command:
summary(mod2_gen_full)


#### Appendix H ####
# Note, reporting 2 tailed tests for all placebo tests
#### Table H1 ####
primary3 <- subset(maindata, year == 1976 & incumbent==1 & 
                    retire__or_resign==0)
primary3$close100 <- ifelse(primary3$distancetodist<=100,1,0)

mod1_trip_placebo <-
  lm_robust(
    numbertrips_actually_taken1q1978  ~ audited * south,
    primary3,
    clusters = primary3$stcd)

mod2_trip_placebo <-
  lm_robust(
    numbertrips_actually_taken1q1978  ~ audited * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100,
    primary3,
    clusters = primary3$stcd)

mod3_trip_placebo <-
  lm_robust(
    numbertrips_actually_taken1q1978  ~ audited,
    primary3,
    clusters = primary3$stcd)

#Table output
modelsummary(list(mod1_trip_placebo, mod2_trip_placebo, mod3_trip_placebo))

# f-stats visible using summary command:
summary(mod1_trip_placebo)
summary(mod2_trip_placebo)
summary(mod3_trip_placebo)

#### Table H2 ####
mod4_trip_placebo <-
  iv_robust(
    numbertrips_actually_taken1q1978  ~ findings * south|
      audited * south,
    primary3,
    clusters = primary3$stcd)

mod5_trip_placebo <-
  iv_robust(
    numbertrips_actually_taken1q1978  ~ findings * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100 | audited * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100,
    primary3,
    clusters = primary3$stcd)

mod6_trip_placebo <-
  iv_robust(
    numbertrips_actually_taken1q1978  ~ findings | audited,
    primary3,
    clusters = primary3$stcd)

#Table output
modelsummary(list(mod4_trip_placebo, mod5_trip_placebo, mod6_trip_placebo))


# f-stats visible using summary command:
summary(mod4_trip_placebo)
summary(mod5_trip_placebo)
summary(mod6_trip_placebo)



#### Table H3 ####
mod1_pcomp_placebo <- lm_robust(pcomp ~ audited * south,
                        primary3,
                        clusters = primary3$stcd)

mod2_pcomp_placebo <- lm_robust(
  pcomp ~ audited * south + dvpinc2p_corrected,
  primary3,
  clusters = primary3$stcd)

mod3_pcomp_placebo <- lm_robust(pcomp ~ audited,
                        primary3,
                        clusters = primary3$stcd)

mod4_prim_placebo <- lm_robust(
  numbercandidatesinprimary ~ audited * south,
  primary3,
  clusters = primary3$stcd)

mod5_prim_placebo <-
  lm_robust(
    numbercandidatesinprimary ~ audited * south + dvpinc2p_corrected,
    primary3,
    clusters = primary3$stcd)

mod6_prim_placebo <- lm_robust(
  numbercandidatesinprimary ~ audited,
  primary3,
  clusters = primary3$stcd)

#Table output
modelsummary(list(mod1_pcomp_placebo, mod2_pcomp_placebo, mod3_pcomp_placebo, 
                  mod4_prim_placebo, mod5_prim_placebo, mod6_prim_placebo))


# f-stats visible using summary command:
summary(mod1_pcomp_placebo)
summary(mod2_pcomp_placebo)
summary(mod3_pcomp_placebo)
summary(mod4_prim_placebo)
summary(mod5_prim_placebo)
summary(mod6_prim_placebo)



#### Table H4 ####
mod7_pcomp_placebo <- iv_robust(pcomp ~ findings * south|
                                  audited * south,
                                primary3,
                                clusters = primary3$stcd)

mod8_pcomp_placebo <- iv_robust(
  pcomp ~ findings * south + dvpinc2p_corrected|
    audited * south + dvpinc2p_corrected,
  primary3,
  clusters = primary3$stcd)

mod9_pcomp_placebo <- iv_robust(pcomp ~ findings|audited,
                                primary3,
                                clusters = primary3$stcd)

mod10_prim_placebo <- iv_robust(
  numbercandidatesinprimary ~ findings * south|audited * south,
  primary3,
  clusters = primary3$stcd)

mod11_prim_placebo <-
  iv_robust(
    numbercandidatesinprimary ~ findings * south + dvpinc2p_corrected|
      audited * south + dvpinc2p_corrected,
    primary3,
    clusters = primary3$stcd)

mod12_prim_placebo <- iv_robust(
  numbercandidatesinprimary ~ findings|audited,
  primary3,
  clusters = primary3$stcd)

#Table output
modelsummary(list(mod7_pcomp_placebo, mod8_pcomp_placebo, mod9_pcomp_placebo, 
                  mod10_prim_placebo, mod11_prim_placebo, mod12_prim_placebo))

# f-stats visible using summary command:
summary(mod7_pcomp_placebo)
summary(mod8_pcomp_placebo)
summary(mod9_pcomp_placebo)
summary(mod10_prim_placebo)
summary(mod11_prim_placebo)
summary(mod12_prim_placebo)



#### Table H5 ####
mod1_gen_placebo <- lm_robust(dvinc2p ~ audited * south,
                    primary3,
                    clusters = primary3$stcd)

mod2_gen_placebo <- lm_robust(dvinc2p ~ audited * south +
                                dvpinc2p_corrected,
                              primary3,
                              clusters = primary3$stcd)

mod3_gen_placebo <- lm_robust(dvinc2p ~ audited,
                              primary3,
                              clusters = primary3$stcd)

#Table output
modelsummary(list(mod1_gen_placebo, mod2_gen_placebo, mod3_gen_placebo))

# f-stats visible using summary command:
summary(mod1_gen_placebo)
summary(mod2_gen_placebo)
summary(mod3_gen_placebo)



#### Table H6 ####
mod4_gen_placebo <- iv_robust(dvinc2p ~ findings * south|
                                audited * south,
                              primary3,
                              clusters = primary3$stcd)

mod5_gen_placebo <- iv_robust(dvinc2p ~ findings * south +
                                dvpinc2p_corrected |audited * south +
                                dvpinc2p_corrected,
                              primary3,
                              clusters = primary3$stcd)

mod6_gen_placebo <- iv_robust(dvinc2p ~ findings|audited,
                              primary3,
                              clusters = primary3$stcd)

#Table output
modelsummary(list(mod4_gen_placebo, mod5_gen_placebo, mod6_gen_placebo))

# f-stats visible using summary command:
summary(mod4_gen_placebo)
summary(mod5_gen_placebo)
summary(mod6_gen_placebo)


#### Appendix I
#### Table I1 ####
mod1_trip <-
  lm_robust(
    numbertrips_actually_taken1q1978  ~ audited * south,
    primary,
    clusters = primary$stcd)


# use tripsdata here since controlling for 1976 trips
mod2_trip <-
  lm_robust(
    numbertrips_actually_taken1q1978  ~ audited * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100,
    tripsdata,
    clusters = tripsdata$stcd)


mod3_trip <-
  lm_robust(
    numbertrips_actually_taken1q1978  ~ audited,
    primary,
    clusters = primary$stcd)

#Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod1_trip, mod2_trip, mod3_trip))

# f-stats visible using summary command:
summary(mod1_trip)
summary(mod2_trip)
summary(mod3_trip)

#### Table I2 ####
mod4_trip <-
  iv_robust(
    numbertrips_actually_taken1q1978  ~ findings * south |
      audited * south,
    primary,
    clusters = primary$stcd)

# note use of tripsdata here
mod5_trip <-
  iv_robust(
    numbertrips_actually_taken1q1978  ~ findings * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100 | audited * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100,
    tripsdata,
    clusters = tripsdata$stcd)


mod6_trip <-
  iv_robust(
    numbertrips_actually_taken1q1978  ~ findings | audited,
    primary,
    clusters = primary$stcd)


#Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod4_trip, mod5_trip, mod6_trip))

# f-stats visible using summary command:
summary(mod4_trip)
summary(mod5_trip)
summary(mod6_trip)


#### Appendix J ####
# Note that we report one-tailed p values throughout, except as discussed in table notes
#### Table J1 ####
mod1_major <- iv_robust(retire__or_resign ~ findings10k*south |
                          audited*south, 
                        retirement,
                        clusters = retirement$stcd)


mod2_major <- iv_robust(retire__or_resign ~ findings10k*south + dvpinc2p_corrected |
                          audited*south + dvpinc2p_corrected , 
                        retirement,
                        clusters = retirement$stcd)


mod3_major <- iv_robust(retire__or_resign ~ findings10k|
                          audited, 
                        retirement,
                        clusters = retirement$stcd)

#Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod1_major, mod2_major, mod3_major))

# f-stats visible using summary command:
summary(mod1_major)
summary(mod2_major)
summary(mod3_major)

#### Table J2 ####

mod4_major <- iv_robust(numbertrips_actually_taken1q1978 ~ findings10k*south |
                          audited*south, 
                        primary,
                        clusters = primary$stcd)

# tripsdata here
mod5_major <- iv_robust(numbertrips_actually_taken1q1978 ~ findings10k*south + 
                          dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
                          distancetodist + close100|
                          audited*south + 
                          dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
                          distancetodist + close100, 
                        tripsdata,
                        clusters = tripsdata$stcd)

mod6_major <- iv_robust(numbertrips_actually_taken1q1978 ~ findings10k|
                          audited, 
                        primary,
                        clusters = primary$stcd)


#Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod4_major, mod5_major, mod6_major))

# f-stats visible using summary command:
summary(mod4_major)
summary(mod5_major)
summary(mod6_major)

#### Table J3 ####
mod7_major <- iv_robust(pcomp ~ findings10k*south |
                          audited*south, 
                        primary,
                        clusters = primary$stcd)


mod8_major <- iv_robust(pcomp ~ findings10k*south + dvpinc2p_corrected |
                          audited*south + dvpinc2p_corrected , 
                        primary,
                        clusters = primary$stcd)


mod9_major <- iv_robust(pcomp ~ findings10k|
                          audited, 
                        primary,
                        clusters = primary$stcd)


mod10_major <- iv_robust(numbercandidatesinprimary ~ findings10k*south |
                          audited*south, 
                        primary,
                        clusters = primary$stcd)


mod11_major <- iv_robust(numbercandidatesinprimary ~ findings10k*south + dvpinc2p_corrected |
                          audited*south + dvpinc2p_corrected , 
                        primary,
                        clusters = primary$stcd)


mod12_major <- iv_robust(numbercandidatesinprimary ~ findings10k|
                          audited, 
                        primary,
                        clusters = primary$stcd)


#Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod7_major, mod8_major, mod9_major, 
                  mod10_major, mod11_major, mod12_major))

# f-stats visible using summary command:
summary(mod7_major)
summary(mod8_major)
summary(mod9_major)
summary(mod10_major)
summary(mod11_major)
summary(mod12_major)

#### Table J4 ####
mod13_major <- iv_robust(dvinc2p ~ findings10k*south |
                          audited*south, 
                        gen, clusters = gen$stcd)


mod14_major <- iv_robust(dvinc2p ~ findings10k*south + dvpinc2p_corrected |
                          audited*south + dvpinc2p_corrected , 
                        gen, clusters = gen$stcd)


mod15_major <- iv_robust(dvinc2p ~ findings10k|
                          audited, 
                        gen, clusters = gen$stcd)

# Table output
# Note that we report one-tailed p values throughout, except as discussed in table notes
modelsummary(list(mod13_major, mod14_major, mod15_major))

# f-stats visible using summary command:
summary(mod13_major)
summary(mod14_major)
summary(mod15_major)
summary(mod16_major)
summary(mod17_major)
summary(mod18_major)


#### APPENDIX L #####

## CCES analysis is in Appendix L.do
## and that .do file uses this data file:  "CCES_cumulative_2006_2018_with_scandals.dta"



##### A FEW ODDS AND ENDS SPECIFICALLY REQUESTED BY JOURNAL FOR REPLICATION PURPOSES ####
# updated violations summary statistics for Appendix A: 
vstats <- subset(maindata, findings == 1)
vstats$exprobcontnum <- vstats$prohibcontribnum + vstats$excesscontribnum
vstats$exprobcontdol <- vstats$prohibcontribdollar + vstats$excesscontribdollar

violationstats <- subset(vstats, select = c(disclosurenum, disclosuredollar, exprobcontnum, 
                                            exprobcontdol, recordnum, recorddollar, misstatenum, 
                                            misstatedol, totdollarviolations, totalviolations))
summary(violationstats)
lapply(violationstats, FUN = "sd")


# substantiating that there were 267 incumbents that were incumbents in both the 94th and 95th congresses
# who did not itemize (this was compiled by RAs by hand and just manifests as NAs in the numbertrips variables, 
# as we explain in the codebook, but the final dataset that meets our critera is the nrows of the dataset)
dim(tripsdata)

# confirming the percent of audited audited legislators with violations:
mean(maindata$findings[maindata$audited == 1 & maindata$year == 1978])
