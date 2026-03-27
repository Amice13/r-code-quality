# This R-file is generating the table SI-4

library(stargazer)
library(xtable)

setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")

load("Data/newspapers.rdata")

## A. Right-wing slant

mod1_rw = lm(score_normalized ~ ih, data=newspapers$front_pages)
mod2_rw = lm(score_normalized ~ ih, data=newspapers$news_pages)
mod3_rw = lm(score_normalized ~ ih, data=newspapers$op_eds)


## B. Positive coverage

mod1_pc = lm(pc_score_normalized ~ ih, data=newspapers$front_pages)
mod2_pc = lm(pc_score_normalized ~ ih, data=newspapers$news_pages)
mod3_pc = lm(pc_score_normalized ~ ih, data=newspapers$op_eds)


stargazer(mod1_rw, mod2_rw, mod3_rw, mod1_pc, mod2_pc, mod3_pc, out="Tables/Table_SI-4.tex")

#print(xtable(comparing_slants, type = "latex", digits=2), file = "Tables/Table_SI-3.tex")
