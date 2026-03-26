# rep4.R: Clarifying issues of divergence between income groups




# Scroll to the very end of code for instructions.





library(foreign)

rm(list = ls())

# Load raw data
d <- read.dta("gp_data.dta")

# Authors say to drop cases of partial change (2.5, 3.5, 4.5) and 99s
d <- subset(d, !(OUTCOME==2.5|OUTCOME==3.5|OUTCOME==4.5|OUTCOME==99))

# Create study's outcome variable from raw OUTCOME measure
d$outcome <- 0
d$outcome[d$OUTCOME>=2 & d$OUTCOME<=4] <- 1

#####

# We're focusing on observations in which the two income groups differ by more than 10 points of avg. support
d.div <- subset(d,abs(pred90_sw-pred50_sw)>0.10)
# Account for "switcher" to make outputs easier to understand later when viewing question text
d.div$adj.outcome <- d.div$outcome
d.div$adj.pred50_sw <- d.div$pred50_sw
d.div$adj.pred90_sw <- d.div$pred90_sw
for (i in 1:nrow(d.div)) {
  if (d.div$switcher[i]==1) {
    d.div$adj.outcome[i] <- 1-d.div$adj.outcome[i]
    d.div$adj.pred50_sw[i] <- 1-d.div$adj.pred50_sw[i]
    d.div$adj.pred90_sw[i] <- 1-d.div$adj.pred90_sw[i]
  }
}

# Foreign policy issue area
d.div.50morefp <- subset(d.div,pred50_sw>pred90_sw & FPYN==1)
d.div.90morefp <- subset(d.div,pred90_sw>pred50_sw & FPYN==1)
median.favor.chg.more.fp <- data.frame(yr=d.div.50morefp$YEAR,q=d.div.50morefp$QuestionText,did.favor.win=d.div.50morefp$adj.outcome,
                        median.support=d.div.50morefp$adj.pred50_sw, wealthy.support=d.div.50morefp$adj.pred90_sw,switcher=d.div.50morefp$switcher)
wealthy.favor.chg.more.fp <- data.frame(yr=d.div.90morefp$YEAR,q=d.div.90morefp$QuestionText,did.favor.win=d.div.90morefp$adj.outcome,
                        median.support=d.div.90morefp$adj.pred50_sw, wealthy.support=d.div.90morefp$adj.pred90_sw,switcher=d.div.90morefp$switcher)
# Don't want favor for change, but rather favor as reported in survey answers, which may mean favoring status quo
median.favor.10ptsmore.fp <- rbind(median.favor.chg.more.fp[median.favor.chg.more.fp$switcher==0,], wealthy.favor.chg.more.fp[wealthy.favor.chg.more.fp$switcher==1,])
row.names(median.favor.10ptsmore.fp)<-NULL
median.favor.10ptsmore.fp$switcher <- NULL
wealthy.favor.10ptsmore.fp <- rbind(wealthy.favor.chg.more.fp[wealthy.favor.chg.more.fp$switcher==0,], median.favor.chg.more.fp[median.favor.chg.more.fp$switcher==1,])
row.names(wealthy.favor.10ptsmore.fp)<-NULL
wealthy.favor.10ptsmore.fp$switcher <- NULL

# Religious/moral issue area
d.div.50morerl <- subset(d.div,pred50_sw>pred90_sw & RLYN==1)
d.div.90morerl <- subset(d.div,pred90_sw>pred50_sw & RLYN==1)
median.favor.chg.more.rl <- data.frame(yr=d.div.50morerl$YEAR,q=d.div.50morerl$QuestionText,did.favor.win=d.div.50morerl$adj.outcome,
                                       median.support=d.div.50morerl$adj.pred50_sw, wealthy.support=d.div.50morerl$adj.pred90_sw,switcher=d.div.50morerl$switcher)
wealthy.favor.chg.more.rl <- data.frame(yr=d.div.90morerl$YEAR,q=d.div.90morerl$QuestionText,did.favor.win=d.div.90morerl$adj.outcome,
                                        median.support=d.div.90morerl$adj.pred50_sw, wealthy.support=d.div.90morerl$adj.pred90_sw,switcher=d.div.90morerl$switcher)
# Don't want favor for change, but rather favor as reported in survey answers, which may mean favoring status quo
median.favor.10ptsmore.rl <- rbind(median.favor.chg.more.rl[median.favor.chg.more.rl$switcher==0,], wealthy.favor.chg.more.rl[wealthy.favor.chg.more.rl$switcher==1,])
row.names(median.favor.10ptsmore.rl)<-NULL
median.favor.10ptsmore.rl$switcher <- NULL
wealthy.favor.10ptsmore.rl <- rbind(wealthy.favor.chg.more.rl[wealthy.favor.chg.more.rl$switcher==0,], median.favor.chg.more.rl[median.favor.chg.more.rl$switcher==1,])
row.names(wealthy.favor.10ptsmore.rl)<-NULL
wealthy.favor.10ptsmore.rl$switcher <- NULL

# Economic issue area
d.div.50moreec <- subset(d.div,pred50_sw>pred90_sw & ECYN==1)
d.div.90moreec <- subset(d.div,pred90_sw>pred50_sw & ECYN==1)
median.favor.chg.more.ec <- data.frame(yr=d.div.50moreec$YEAR,q=d.div.50moreec$QuestionText,did.favor.win=d.div.50moreec$adj.outcome,
                                       median.support=d.div.50moreec$adj.pred50_sw, wealthy.support=d.div.50moreec$adj.pred90_sw,switcher=d.div.50moreec$switcher)
wealthy.favor.chg.more.ec <- data.frame(yr=d.div.90moreec$YEAR,q=d.div.90moreec$QuestionText,did.favor.win=d.div.90moreec$adj.outcome,
                                        median.support=d.div.90moreec$adj.pred50_sw, wealthy.support=d.div.90moreec$adj.pred90_sw,switcher=d.div.90moreec$switcher)
# Don't want favor for change, but rather favor as reported in survey answers, which may mean favoring status quo
median.favor.10ptsmore.ec <- rbind(median.favor.chg.more.ec[median.favor.chg.more.ec$switcher==0,], wealthy.favor.chg.more.ec[wealthy.favor.chg.more.ec$switcher==1,])
row.names(median.favor.10ptsmore.ec)<-NULL
median.favor.10ptsmore.ec$switcher <- NULL
wealthy.favor.10ptsmore.ec <- rbind(wealthy.favor.chg.more.ec[wealthy.favor.chg.more.ec$switcher==0,], median.favor.chg.more.ec[median.favor.chg.more.ec$switcher==1,])
row.names(wealthy.favor.10ptsmore.ec)<-NULL
wealthy.favor.10ptsmore.ec$switcher <- NULL

# Social welfare issue area
d.div.50moresw <- subset(d.div,pred50_sw>pred90_sw & SWYN==1)
d.div.90moresw <- subset(d.div,pred90_sw>pred50_sw & SWYN==1)
median.favor.chg.more.sw <- data.frame(yr=d.div.50moresw$YEAR,q=d.div.50moresw$QuestionText,did.favor.win=d.div.50moresw$adj.outcome,
                                       median.support=d.div.50moresw$adj.pred50_sw, wealthy.support=d.div.50moresw$adj.pred90_sw,switcher=d.div.50moresw$switcher)
wealthy.favor.chg.more.sw <- data.frame(yr=d.div.90moresw$YEAR,q=d.div.90moresw$QuestionText,did.favor.win=d.div.90moresw$adj.outcome,
                                        median.support=d.div.90moresw$adj.pred50_sw, wealthy.support=d.div.90moresw$adj.pred90_sw,switcher=d.div.90moresw$switcher)
# Don't want favor for change, but rather favor as reported in survey answers, which may mean favoring status quo
median.favor.10ptsmore.sw <- rbind(median.favor.chg.more.sw[median.favor.chg.more.sw$switcher==0,], wealthy.favor.chg.more.sw[wealthy.favor.chg.more.sw$switcher==1,])
row.names(median.favor.10ptsmore.sw)<-NULL
median.favor.10ptsmore.sw$switcher <- NULL
wealthy.favor.10ptsmore.sw <- rbind(wealthy.favor.chg.more.sw[wealthy.favor.chg.more.sw$switcher==0,], median.favor.chg.more.sw[median.favor.chg.more.sw$switcher==1,])
row.names(wealthy.favor.10ptsmore.sw)<-NULL
wealthy.favor.10ptsmore.sw$switcher <- NULL



####################################################
####################################################

# How to use this code

# 1. Run everything up to double pound-sign line divider just above.
# 2. There are 4 issue areas: foreign policy (fp), religious/moral (rl), economic (ec), and social welfare (sw).
# 3. For each issue area, you can see either the questions that median-income respondents answered "favor" 
#       at a 10% higher rate than the wealthy, or the opposite.
# 4. For example, View(wealthy.favor.10ptsmore.fp) produces a readable list of the foreign policy questions 
#       in the dataset for which the wealthy answered "favor" more often 
#       (by at least a10% margin of imputed preference).
# 6. Each data frame notes the year of a question, whether or not an answer equivalent to "favor" won the day 
#       in terms of policy outcome, and imputed support levels of the two groups (accounting for "switcher" in 
#       the authors' dataset, for those familiar).
# 7. Important: these lists do not capture only "disagreement" on issues, but rather "divergence." 
#       The median-income and wealthy can be on the same side of an issue and still diverge by 10 points, so
#       just seeing that "favor" was reflected in the outcome does not mean that the side with higher relative
#       support got its way.
# 8. If questions are cut off, this reflects what is in the original dataset, but searching online using the 
#       limited wording can sometimes yield full questions (pay attention to year of survey, though).

# NOTE: RStudio (free download) is best for View() function since it does not cut off long text entries
#       like R does. If you don't have RStudio, uncomment the block at the end of the file to output 
#       files for viewing in Excel.

# Your viewing options, with notes at right about which issues affect multiple observations (incomplete list):
View(median.favor.10ptsmore.fp)      # Trade protection w.r.t. Japan (Note higher support for Iraq war and some serious civil liberties restrictions)
View(wealthy.favor.10ptsmore.fp)     # NAFTA and foreign assistance
View(median.favor.10ptsmore.rl)      # Abortion issue 
View(wealthy.favor.10ptsmore.rl)     # Abortion issue
View(median.favor.10ptsmore.ec)
View(wealthy.favor.10ptsmore.ec)     # Tax reform
View(median.favor.10ptsmore.sw)      # Health care
View(wealthy.favor.10ptsmore.sw)

# IF NEEDED: For viewing these outputs in Excel (csv): 
# write.table(median.favor.10ptsmore.fp, file = "medianfavorFP.csv", append = FALSE, quote = TRUE, sep = ",", row.names = FALSE)
# write.table(wealthy.favor.10ptsmore.fp, file = "wealthyfavorFP.csv", append = FALSE, quote = TRUE, sep = ",", row.names = FALSE)
# write.table(median.favor.10ptsmore.rl, file = "medianfavorRL.csv", append = FALSE, quote = TRUE, sep = ",", row.names = FALSE)
# write.table(wealthy.favor.10ptsmore.rl, file = "wealthyfavorRL.csv", append = FALSE, quote = TRUE, sep = ",", row.names = FALSE)
# write.table(median.favor.10ptsmore.ec, file = "medianfavorEC.csv", append = FALSE, quote = TRUE, sep = ",", row.names = FALSE)
# write.table(wealthy.favor.10ptsmore.ec, file = "wealthyfavorEC.csv", append = FALSE, quote = TRUE, sep = ",", row.names = FALSE)
# write.table(median.favor.10ptsmore.sw, file = "medianfavorSW.csv", append = FALSE, quote = TRUE, sep = ",", row.names = FALSE)
# write.table(wealthy.favor.10ptsmore.sw, file = "wealthyfavorSW.csv", append = FALSE, quote = TRUE, sep = ",", row.names = FALSE)


