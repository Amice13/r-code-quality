####################
### Wood & Grose ###
####################

# Preliminary note: to keep the object list from getting annoying, we re-use object names in generating
# confidence intervals and figures generated from resampling. 
# So just be sure to re-run the entire section of code
# if you need to re-generate confidence intervals and figures.  

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

load("wood_grose_main_data.RData")


#### TABLE 1 ####
retirement <- subset(maindata, year == 1978 &
                       !(maindata$name %in% c("LIVINGSTON", "GREEN", "GARCIA")))


mod1_retire <- lm_robust(
  retire__or_resign ~ audited * south,
  retirement,
  clusters = retirement$stcd,
  alpha = 0.1
)

mod2_retire <-
  lm_robust(
    retire__or_resign ~ audited * south + dvpinc2p_corrected + seniority,
    retirement,
    clusters = retirement$stcd,
    alpha = 0.1
  )

mod3_retire <- lm_robust(retire__or_resign ~ audited,
                         retirement,
                         clusters = retirement$stcd,
                         alpha = 0.1)

#### TABLE 2 ####

mod4_retire <- iv_robust(
  retire__or_resign ~ findings * south |
    audited * south,
  retirement,
  clusters = retirement$stcd,
  alpha = 0.1
)

mod5_retire <-
  iv_robust(
    retire__or_resign ~ findings * south + dvpinc2p_corrected + seniority |
      audited * south + dvpinc2p_corrected + seniority,
    retirement,
    clusters = retirement$stcd,
    alpha = 0.1
  )

mod6_retire <- iv_robust(
  retire__or_resign ~ findings |
    audited,
  retirement,
  clusters = retirement$stcd,
  alpha = 0.1
)

#### TABLE 3 ####
primary <- subset(maindata, year == 1978 &
                    incumbent_primary == 1 &
                    !(name %in% c("TONRY", "KOCH", "BADILLO")))

mod1_pcomp <- lm_robust(pcomp ~ audited * south,
                        primary,
                        clusters = primary$stcd,
                        alpha = 0.1)

mod2_pcomp <- lm_robust(
  pcomp ~ audited * south + dvpinc2p_corrected,
  primary,
  clusters = primary$stcd,
  alpha = 0.1
)

mod3_pcomp <- lm_robust(pcomp ~ audited,
                        primary,
                        clusters = primary$stcd,
                        alpha = 0.1)

mod4_prim <- lm_robust(
  numbercandidatesinprimary ~ audited * south,
  primary,
  clusters = primary$stcd,
  alpha = 0.1
)

mod5_prim <-
  lm_robust(
    numbercandidatesinprimary ~ audited * south + dvpinc2p_corrected,
    primary,
    clusters = primary$stcd,
    alpha = 0.1
  )

mod6_prim <- lm_robust(
  numbercandidatesinprimary ~ audited,
  primary,
  clusters = primary$stcd,
  alpha = 0.1
)

#### TABLE 4 ####
mod7_pcomp <- iv_robust(
  pcomp ~ findings * south |
    audited * south,
  primary,
  clusters = primary$stcd,
  alpha = 0.1
)

mod8_pcomp <-
  iv_robust(
    pcomp ~ findings * south + dvpinc2p_corrected |
      audited * south + dvpinc2p_corrected,
    primary,
    clusters = primary$stcd,
    alpha = 0.1
  )

mod9_pcomp <- iv_robust(pcomp ~ findings |
                          audited,
                        primary,
                        clusters = primary$stcd,
                        alpha = 0.1)

mod10_prim <- iv_robust(
  numbercandidatesinprimary ~ findings * south |
    audited * south,
  primary,
  clusters = primary$stcd,
  alpha = 0.1
)

mod11_prim <-
  iv_robust(
    numbercandidatesinprimary ~ findings * south + dvpinc2p_corrected |
      audited * south + dvpinc2p_corrected,
    primary,
    clusters = primary$stcd,
    alpha = 0.1
  )

mod12_prim <- iv_robust(
  numbercandidatesinprimary ~ findings |
    audited,
  primary,
  clusters = primary$stcd,
  alpha = 0.1
)


#### TABLE 5 ####
gen <- subset(maindata, year == 1978 & incumbent == 1 & 
                !(name %in% c("TONRY", "KOCH", "BADILLO")))

mod1_gen <- lm_robust(dvinc2p ~ audited * south,
                      gen,
                      clusters = gen$stcd,
                      alpha = 0.1)

mod2_gen <- lm_robust(
  dvinc2p ~ audited * south + dvpinc2p_corrected,
  gen,
  clusters = gen$stcd,
  alpha = 0.1
)

mod3_gen <- lm_robust(dvinc2p ~ audited,
                      gen,
                      clusters = gen$stcd,
                      alpha = 0.1)


##### TABLE 6 ####
mod4_gen <- iv_robust(
  dvinc2p ~ findings * south |
    audited * south,
  gen,
  clusters = gen$stcd,
  alpha = 0.1
)

mod5_gen <-
  iv_robust(
    dvinc2p ~ findings * south + dvpinc2p_corrected |
      audited * south + dvpinc2p_corrected,
    gen,
    clusters = gen$stcd,
    alpha = 0.1
  )

mod6_gen <- iv_robust(dvinc2p ~ findings |
                        audited,
                      gen,
                      clusters = gen$stcd,
                      alpha = 0.1)

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
## and we merge withour scandal data for a dummy variable to identify when the district has a campaign finance scandal (cf_scandal)

## here's the code to use to replicate the advertising t.tests, 
# but it's commented out so it doesn't break when you run the .R file
#diff1 <- t.test(rep_ads$neg[rep_ads$issue91 == 1], rep_ads$neg[rep_ads$issue91 == 0])
#diff2 <- t.test(rep_ads$negscandal[rep_ads$cfscandal == 1], rep_ads$negscandal[rep_ads$cfscandal == 0])  
#diff3 <- t.test(rep_ads$neg[rep_ads$issue91 == 1 & rep_ads$cfscandal == 1 & rep_ads$inc_targeted == 1], 
#       rep_ads$neg[rep_ads$issue91 == 1 & rep_ads$cfscandal == 0 & rep_ads$inc_targeted == 1])
#diff4 <- t.test(rep_ads$neg[rep_ads$cfscandal == 1 & rep_ads$election == "PRIMARY"], 
#                rep_ads$neg[rep_ads$cfscandal == 0 & rep_ads$election == "PRIMARY"])  

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

#### FIGURE 3 ####
#FIGURE 3a#
retire <- data.frame(
  model = rep(c("Audits", "Violations"), each = 3),
  variable = rep(c(
    "Not South, Audit", "South, No Audit", "South, Audit"
  ), 2),
  coef = c(coef(mod1_retire)[2:4], coef(mod4_retire)[2:4]),
  ymin = c(
    summary(mod1_retire)$coefficients[2:4, 5],
    summary(mod4_retire)$coefficients[2:4, 5]
  ),
  ymax = c(
    summary(mod1_retire)$coefficients[2:4, 6],
    summary(mod4_retire)$coefficients[2:4, 6]
  )
)

retire$variable <- factor(retire$variable,
                          levels = c("Not South, Audit", "South, Audit", "South, No Audit"))
pdf(file = "figure3a.pdf",
    height = 5,
    width = 5)
ggplot(retire, aes(
  x = variable,
  y = coef,
  ymax = ymax,
  ymin = ymin
)) +
  geom_point(size = 2) + geom_errorbar(width = 0) + facet_wrap( ~ model) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) + xlab("") +
  ylab("Change in Probability of Retirement") +
  theme(axis.text = element_text(angle = 90), panel.grid = element_blank())
dev.off()


#FIGURE 3b#
# here we use the "primary" data since we aren't controlling for lagged trips in 1976
mod1_trip <-
  lm_robust(
    numbertrips_actually_taken1q1978  ~ audited * south,
    primary,
    clusters = primary$stcd,
    alpha = 0.1
  )
mod2_trip <-
  iv_robust(
    numbertrips_actually_taken1q1978 ~ findings * south |
      audited * south,
    primary,
    clusters = primary$stcd,
    alpha = 0.1
  )

trips <- data.frame(
  model = rep(c("Audits", "Violations"), each = 3),
  variable = rep(c(
    "Not South, Audit", "South, No Audit", "South, Audit"
  ), 2),
  coef = c(coef(mod1_trip)[2:4], coef(mod2_trip)[2:4]),
  ymin = c(
    summary(mod1_trip)$coefficients[2:4, 5],
    summary(mod2_trip)$coefficients[2:4, 5]
  ),
  ymax = c(
    summary(mod1_trip)$coefficients[2:4, 6],
    summary(mod2_trip)$coefficients[2:4, 6]
  )
)

trips$variable <- factor(trips$variable,
                         levels = c("Not South, Audit", "South, Audit", "South, No Audit"))

pdf(file = "figure3b.pdf",
    height = 5,
    width = 5)
ggplot(trips, aes(
  x = variable,
  y = coef,
  ymax = ymax,
  ymin = ymin
)) +
  geom_point(size = 2) + geom_errorbar(width = 0) + facet_wrap( ~ model) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) + xlab("") +
  ylab("Change in Number of Trips Home") +
  theme(axis.text = element_text(angle = 90), panel.grid = element_blank())
dev.off()

#FIGURE 3c#
primaries_comp <- data.frame(
  model = rep(c("Audits", "Violations"), each = 3),
  variable = rep(c(
    "Not South, Audit", "South, No Audit", "South, Audit"
  ), 2),
  coef = c(coef(mod1_pcomp)[2:4], coef(mod7_pcomp)[2:4]),
  ymin = c(
    summary(mod1_pcomp)$coefficients[2:4, 5],
    summary(mod7_pcomp)$coefficients[2:4, 5]
  ),
  ymax = c(
    summary(mod1_pcomp)$coefficients[2:4, 6],
    summary(mod7_pcomp)$coefficients[2:4, 6]
  )
)


pdf(file = "figure3c1.pdf",
    height = 5,
    width = 5)
ggplot(primaries_comp, aes(
  x = variable,
  y = coef,
  ymax = ymax,
  ymin = ymin
)) +
  geom_point(size = 2) +
  geom_errorbar(width = 0) +
  facet_wrap( ~ model) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) + xlab("") +
  ylab("Change in Primary Competition") +
  theme(axis.text = element_text(angle = 90)) +
  scale_shape_manual(name = "", values = c(1, 2)) +
  theme(legend.position = "bottom", panel.grid = element_blank())
dev.off()

primaries_num <- data.frame(
  model = rep(c("Audits", "Violations"), each = 3),
  variable = rep(c(
    "Not South, Audit", "South, No Audit", "South, Audit"
  ), 2),
  coef = c(coef(mod4_prim)[2:4], coef(mod10_prim)[2:4]),
  ymin = c(
    summary(mod4_prim)$coefficients[2:4, 5],
    summary(mod10_prim)$coefficients[2:4, 5]
  ),
  ymax = c(
    summary(mod4_prim)$coefficients[2:4, 6],
    summary(mod10_prim)$coefficients[2:4, 6]
  )
)


pdf(file = "figure3c2.pdf",
    height = 5,
    width = 5)
ggplot(primaries_num, aes(
  x = variable,
  y = coef,
  ymax = ymax,
  ymin = ymin
)) +
  geom_point(size = 2) +
  geom_errorbar(width = 0) +
  facet_wrap( ~ model) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) + xlab("") +
  ylab("Change in Number of Primary Challengers") +
  theme(axis.text = element_text(angle = 90)) +
  scale_shape_manual(name = "", values = c(1, 2)) +
  theme(legend.position = "bottom", panel.grid = element_blank())
dev.off()

#FIGURE 3d#
gen1 <- data.frame(
  model = rep(c("Audits", "Violations"), each = 3),
  variable = rep(c(
    "Not South, Audit", "South, No Audit", "South, Audit"
  ), 2),
  coef = c(coef(mod1_gen)[2:4], coef(mod4_gen)[2:4]),
  ymin = c(
    summary(mod1_gen)$coefficients[2:4, 5],
    summary(mod4_gen)$coefficients[2:4, 5]
  ),
  ymax = c(
    summary(mod1_gen)$coefficients[2:4, 6],
    summary(mod4_gen)$coefficients[2:4, 6]
  )
)

gen1$variable <- factor(gen1$variable,
                       levels = c("Not South, Audit", "South, Audit", "South, No Audit"))

pdf(file = "figure3d.pdf",
    height = 5,
    width = 5)
ggplot(gen1, aes(
  x = variable,
  y = coef,
  ymax = ymax,
  ymin = ymin
)) +
  geom_point(size = 2) + geom_errorbar(width = 0) + facet_wrap( ~ model) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) + xlab("") +
  ylab("Change in General Election Vote Share") +
  theme(axis.text = element_text(angle = 90), panel.grid = element_blank())
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
                                      retirement$name!="BADILLO",], alpha = 0.1)
mod_b2 <- lm_robust(totalaudithits_all ~ findings10k + totalhits, 
                    data = retirement[retirement$name!="TONRY" &
                                        retirement$name!="KOCH" & 
                                        retirement$name!="BADILLO",], alpha = 0.1)
#### Appendix C ####
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

#### Appendix D ####
#model 1
mod1_retire_logit <- glm(
  retire__or_resign ~ audited * south,
  retirement,
  family="binomial",
)
# robust ses
mod1_retire_logit_se <- coeftest(mod1_retire_logit, vcov = vcovHC(mod1_retire_logit, type = "HC1"))
# calculate 90%CIs
est <- mod1_retire_logit_se[,1]
se <-  mod1_retire_logit_se[,2]
ci.low <- est - 1.645*se
ci.high <- est + 1.645*se

# model 2
mod2_retire_logit <- glm(
  retire__or_resign ~ audited * south + dvpinc2p_corrected + seniority,
  retirement,
  family="binomial",
)
# robust ses
mod2_retire_logit_se <- coeftest(mod2_retire_logit, vcov = vcovHC(mod2_retire_logit, type = "HC1"))
# calculate 90%CIs
est <- mod2_retire_logit_se[,1]
se <-  mod2_retire_logit_se[,2]
ci.low <- est - 1.645*se
ci.high <- est + 1.645*se

# model 3
mod3_retire_logit <- glm(
  retire__or_resign ~ audited,
  retirement,
  family="binomial",
)
#robust ses
mod3_retire_logit_se <- coeftest(mod3_retire_logit, vcov = vcovHC(mod3_retire_logit, type = "HC1"))
# calculate 90%CIs
est <- mod3_retire_logit_se[,1]
se <-  mod3_retire_logit_se[,2]
ci.low <- est - 1.645*se
ci.high <- est + 1.645*se

#### Appendix E ####
primary$close100 <- ifelse(primary$distancetodist<=100,1,0)
tripsdata <- subset(primary, complete.cases(numbertrips_actually_taken1q1976))


# model 1 
# use "primary" data because no lagged trips on RHS
mod1_trip_nb <- glm.nb(numbertrips_actually_taken1q1978  ~ audited * south,
                       primary)
# robust SEs, model 2
mod1_trip_nb_se <- coeftest(mod1_trip_nb, vcov = vcovHC(mod1_trip_nb, type = "HC1"))

# 90% CIs
est <- mod1_trip_nb_se[,1]
se <-  mod1_trip_nb_se[,2]
ci.low <- est - 1.645*se
ci.high <- est + 1.645*se

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

# 90% CIs
est <- mod2_trip_nb_se[,1]
se <-  mod2_trip_nb_se[,2]
ci.low <- est - 1.645*se
ci.high <- est + 1.645*se

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

# 90% CIs
est <- mod3_trip_nb_se[,1]
se <-  mod3_trip_nb_se[,2]
ci.low <- est - 1.645*se
ci.high <- est + 1.645*se

# predicted values
summary(allEffects(mod3_trip_nb))



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

#### Table G1 ####
mod1_retire_full <-
  lm_robust(
    retire__or_resign ~ audited * south + dvpinc2p_corrected + seniority +
      comm_chair + leaders + scandal78,
    retirement,
    clusters = retirement$stcd,
    alpha = 0.1
  )

mod2_retire_full <-
  lm_robust(
    retire__or_resign ~ audited * south + dvpinc2p_corrected + ageinyears1976 +
      comm_chair + leaders + scandal78,
    retirement,
    clusters = retirement$stcd,
    alpha = 0.1
  )

#### Table G2 ####
mod3_retire_full <- iv_robust(retire__or_resign  ~ findings*south 
          + dvpinc2p_corrected + seniority + comm_chair + leaders + scandal78| 
            audited*south + dvpinc2p_corrected + seniority +
            leaders + scandal78 + comm_chair,  
          data = retirement, alpha = 0.1, clusters = retirement$stcd)

mod4_retire_full <- iv_robust(retire__or_resign  ~ findings*south 
                    + dvpinc2p_corrected + ageinyears1976 + comm_chair + leaders + scandal78| 
                    audited*south + dvpinc2p_corrected + ageinyears1976 +
                    leaders + scandal78 + comm_chair,  
                    data = retirement, alpha = 0.1, clusters = retirement$stcd)

#### Table G3 ####
mod1_trip_full <-
  lm_robust(
    numbertrips_actually_taken1q1978  ~ audited * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100 + qualchvsinc + oppo_minusinc_exp + scandal78,
    tripsdata,
    clusters = tripsdata$stcd,
    alpha = 0.1
  )

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
    clusters = tripsdata$stcd,
    alpha = 0.1
  )

#### Table G5 ####
mod1_pcomp_full <- lm_robust(pcomp ~ audited * south +
                               dvpinc2p_corrected + scandal78,
                      primary, clusters = primary$stcd,
                                           alpha = 0.1)

mod1_prim_full <- lm_robust(numbercandidatesinprimary ~ audited * south +
                               dvpinc2p_corrected + scandal78,
                             primary, clusters = primary$stcd,
                             alpha = 0.1)

#### Table G6 ####
mod2_pcomp_full <- iv_robust(pcomp ~ findings * south +
                               dvpinc2p_corrected + scandal78|
                               audited * south +
                               dvpinc2p_corrected + scandal78,
                             primary, clusters = primary$stcd,
                             alpha = 0.1)

mod2_prim_full <- iv_robust(numbercandidatesinprimary ~ findings * south +
                              dvpinc2p_corrected + scandal78|
                              audited * south +
                              dvpinc2p_corrected + scandal78,
                            primary, clusters = primary$stcd,
                            alpha = 0.1)

#### Table G7 ####
mod1_gen_full <- lm_robust(dvinc2p ~ audited * south + dvpinc2p_corrected +
                             qualchvsinc + oppo_minusinc_exp + unoppo +
                             scandal78,
                           gen,
                           clusters = gen$stcd,
                           alpha = 0.1)

#### Table G8 ####
mod2_gen_full <- iv_robust(dvinc2p ~ findings * south + dvpinc2p_corrected +
                             qualchvsinc + oppo_minusinc_exp + unoppo +
                             scandal78 | audited * south + dvpinc2p_corrected +
                             qualchvsinc + oppo_minusinc_exp + unoppo +
                             scandal78,
                           gen,
                           clusters = gen$stcd,
                           alpha = 0.1)


#### Appendix H ####

#### Table H1 ####
primary3 <- subset(maindata, year == 1976 & incumbent==1 & 
                    retire__or_resign==0)
primary3$close100 <- ifelse(primary3$distancetodist<=100,1,0)

mod1_trip_placebo <-
  lm_robust(
    numbertrips_actually_taken1q1978  ~ audited * south,
    primary3,
    clusters = primary3$stcd,
    alpha = 0.1
  )

mod2_trip_placebo <-
  lm_robust(
    numbertrips_actually_taken1q1978  ~ audited * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100,
    primary3,
    clusters = primary3$stcd,
    alpha = 0.1
  )

mod3_trip_placebo <-
  lm_robust(
    numbertrips_actually_taken1q1978  ~ audited,
    primary3,
    clusters = primary3$stcd,
    alpha = 0.1
  )

#### Table H2 ####
mod4_trip_placebo <-
  iv_robust(
    numbertrips_actually_taken1q1978  ~ findings * south|
      audited * south,
    primary3,
    clusters = primary3$stcd,
    alpha = 0.1
  )

mod5_trip_placebo <-
  iv_robust(
    numbertrips_actually_taken1q1978  ~ findings * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100 | audited * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100,
    primary3,
    clusters = primary3$stcd,
    alpha = 0.1
  )

mod6_trip_placebo <-
  iv_robust(
    numbertrips_actually_taken1q1978  ~ findings | audited,
    primary3,
    clusters = primary3$stcd,
    alpha = 0.1
  )

#### Table H3 ####
mod1_pcomp_placebo <- lm_robust(pcomp ~ audited * south,
                        primary3,
                        clusters = primary3$stcd,
                        alpha = 0.1)

mod2_pcomp_placebo <- lm_robust(
  pcomp ~ audited * south + dvpinc2p_corrected,
  primary3,
  clusters = primary3$stcd,
  alpha = 0.1
)

mod3_pcomp_placebo <- lm_robust(pcomp ~ audited,
                        primary3,
                        clusters = primary3$stcd,
                        alpha = 0.1)

mod4_prim_placebo <- lm_robust(
  numbercandidatesinprimary ~ audited * south,
  primary3,
  clusters = primary3$stcd,
  alpha = 0.1
)

mod5_prim_placebo <-
  lm_robust(
    numbercandidatesinprimary ~ audited * south + dvpinc2p_corrected,
    primary3,
    clusters = primary3$stcd,
    alpha = 0.1
  )

mod6_prim_placebo <- lm_robust(
  numbercandidatesinprimary ~ audited,
  primary3,
  clusters = primary3$stcd,
  alpha = 0.1
)

#### Table H4 ####
mod7_pcomp_placebo <- iv_robust(pcomp ~ findings * south|
                                  audited * south,
                                primary3,
                                clusters = primary3$stcd,
                                alpha = 0.1)

mod8_pcomp_placebo <- iv_robust(
  pcomp ~ findings * south + dvpinc2p_corrected|
    audited * south + dvpinc2p_corrected,
  primary3,
  clusters = primary3$stcd,
  alpha = 0.1
)

mod9_pcomp_placebo <- iv_robust(pcomp ~ findings|audited,
                                primary3,
                                clusters = primary3$stcd,
                                alpha = 0.1)

mod10_prim_placebo <- iv_robust(
  numbercandidatesinprimary ~ findings * south|audited * south,
  primary3,
  clusters = primary3$stcd,
  alpha = 0.1
)

mod11_prim_placebo <-
  iv_robust(
    numbercandidatesinprimary ~ findings * south + dvpinc2p_corrected|
      audited * south + dvpinc2p_corrected,
    primary3,
    clusters = primary3$stcd,
    alpha = 0.1
  )

mod12_prim_placebo <- iv_robust(
  numbercandidatesinprimary ~ findings|audited,
  primary3,
  clusters = primary3$stcd,
  alpha = 0.1
)

#### Table H5 ####
mod1_gen_placebo <- lm_robust(dvinc2p ~ audited * south,
                    primary3,
                    clusters = primary3$stcd,
                    alpha = 0.1)
mod2_gen_placebo <- lm_robust(dvinc2p ~ audited * south +
                                dvpinc2p_corrected,
                              primary3,
                              clusters = primary3$stcd,
                              alpha = 0.1)
mod3_gen_placebo <- lm_robust(dvinc2p ~ audited,
                              primary3,
                              clusters = primary3$stcd,
                              alpha = 0.1)

#### Table H6 ####
mod4_gen_placebo <- iv_robust(dvinc2p ~ findings * south|
                                audited * south,
                              primary3,
                              clusters = primary3$stcd,
                              alpha = 0.1)
mod5_gen_placebo <- iv_robust(dvinc2p ~ findings * south +
                                dvpinc2p_corrected |audited * south +
                                dvpinc2p_corrected,
                              primary3,
                              clusters = primary3$stcd,
                              alpha = 0.1)
mod6_gen_placebo <- iv_robust(dvinc2p ~ findings|audited,
                              primary3,
                              clusters = primary3$stcd,
                              alpha = 0.1)

#### Appendix I ####

#### Table I1 ####
mod1_trip <-
  lm_robust(
    numbertrips_actually_taken1q1978  ~ audited * south,
    primary,
    clusters = primary$stcd,
    alpha = 0.1
  )

# use tripsdata since controlling for 1976 trips
mod2_trip <-
  lm_robust(
    numbertrips_actually_taken1q1978  ~ audited * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100,
    tripsdata,
    clusters = tripsdata$stcd,
    alpha = 0.1
  )
mod3_trip <-
  lm_robust(
    numbertrips_actually_taken1q1978  ~ audited,
    primary,
    clusters = primary$stcd,
    alpha = 0.1
  )

#### Table I2 ####
mod4_trip <-
  iv_robust(
    numbertrips_actually_taken1q1978  ~ findings * south |
      audited * south,
    primary,
    clusters = primary$stcd,
    alpha = 0.1
  )
# note use of tripsdata here
mod5_trip <-
  iv_robust(
    numbertrips_actually_taken1q1978  ~ findings * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100 | audited * south +
      dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
      distancetodist + close100,
    tripsdata,
    clusters = tripsdata$stcd,
    alpha = 0.1
  )
mod6_trip <-
  iv_robust(
    numbertrips_actually_taken1q1978  ~ findings | audited,
    primary,
    clusters = primary$stcd,
    alpha = 0.1
  )


#### Appendix J ####

#### Table J1 ####
mod1_major <- iv_robust(retire__or_resign ~ findings10k*south |
                          audited*south, 
                        retirement,
                        clusters = retirement$stcd,
                        alpha = .1)
mod2_major <- iv_robust(retire__or_resign ~ findings10k*south + dvpinc2p_corrected |
                          audited*south + dvpinc2p_corrected , 
                        retirement,
                        clusters = retirement$stcd,
                        alpha = .1)
mod3_major <- iv_robust(retire__or_resign ~ findings10k|
                          audited, 
                        retirement,
                        clusters = retirement$stcd,
                        alpha = .1)

#### Table J2 ####

mod4_major <- iv_robust(numbertrips_actually_taken1q1978 ~ findings10k*south |
                          audited*south, 
                        primary,
                        clusters = primary$stcd,
                        alpha = .1)
# tripsdata here
mod5_major <- iv_robust(numbertrips_actually_taken1q1978 ~ findings10k*south + 
                          dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
                          distancetodist + close100|
                          audited*south + 
                          dvpinc2p_corrected + seniority + numbertrips_actually_taken1q1976 +
                          distancetodist + close100, 
                        tripsdata,
                        clusters = tripsdata$stcd,
                        alpha = .1)

mod6_major <- iv_robust(numbertrips_actually_taken1q1978 ~ findings10k|
                          audited, 
                        primary,
                        clusters = primary$stcd,
                        alpha = .1)

#### Table J3 ####
mod7_major <- iv_robust(pcomp ~ findings10k*south |
                          audited*south, 
                        primary,
                        clusters = primary$stcd,
                        alpha = .1)
mod8_major <- iv_robust(pcomp ~ findings10k*south + dvpinc2p_corrected |
                          audited*south + dvpinc2p_corrected , 
                        primary,
                        clusters = primary$stcd,
                        alpha = .1)
mod9_major <- iv_robust(pcomp ~ findings10k|
                          audited, 
                        primary,
                        clusters = primary$stcd,
                        alpha = .1)
mod10_major <- iv_robust(numbercandidatesinprimary ~ findings10k*south |
                          audited*south, 
                        primary,
                        clusters = primary$stcd,
                        alpha = .1)
mod11_major <- iv_robust(numbercandidatesinprimary ~ findings10k*south + dvpinc2p_corrected |
                          audited*south + dvpinc2p_corrected , 
                        primary,
                        clusters = primary$stcd,
                        alpha = .1)
mod12_major <- iv_robust(numbercandidatesinprimary ~ findings10k|
                          audited, 
                        primary,
                        clusters = primary$stcd,
                        alpha = .1)

#### Table J4 ####
mod13_major <- iv_robust(dvinc2p ~ findings10k*south |
                          audited*south, 
                        gen,
                        clusters = gen$stcd,
                        alpha = .1)
mod14_major <- iv_robust(dvinc2p ~ findings10k*south + dvpinc2p_corrected |
                          audited*south + dvpinc2p_corrected , 
                        gen,
                        clusters = gen$stcd,
                        alpha = .1)
mod15_major <- iv_robust(dvinc2p ~ findings10k|
                          audited, 
                        gen,
                        clusters = gen$stcd,
                        alpha = .1)


#### APPENDIX L #####

## CCES analysis is in Appendix L.do
## and that .do file uses this data file:  "CCES_cumulative_2006_2018_with_scandals.dta"


                        