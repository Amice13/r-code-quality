#### Replication file
#### Hopkins, Kaiser & Perez
#### Appendix Table A9

### must reset working directory
setwd("/users/danhop/Dropbox/PDparty/rcode/replication/")

#dtam2sub <- subset(dtam2,select=c("FTREPS.W1","FTREPS.W2","FTREPS.W3",
#                                  "FTDEMS.W1","FTDEMS.W2","FTDEMS.W3",
#                                  "PATHWAY1","NATORIDX.W1","EDYEARS.W1","HISPANIC.W1",
#                                  "SPANISH.W1","FEMALE.W1","AGE.W1","INCOMER.W1","PATHWAY3"))

#save(dtam2sub,file="a9-replication-data-12212022.Rdata")

load("a9-replication-data-12212022.Rdata")

lout1wave.reps <- lm(FTREPS.W1 ~ PATHWAY1+NATORIDX.W1+ EDYEARS.W1+HISPANIC.W1+SPANISH.W1+
                       FEMALE.W1+AGE.W1+INCOMER.W1,data=dtam2sub[! dtam2sub$PATHWAY3 %in% c(NA),])
lout2wave.reps <- lm(FTREPS.W2 ~ PATHWAY1+NATORIDX.W1+ EDYEARS.W1+HISPANIC.W1+SPANISH.W1+
                       FEMALE.W1+AGE.W1+INCOMER.W1,data=dtam2sub[! dtam2sub$PATHWAY3 %in% c(NA),])
lout3wave.reps <- lm(FTREPS.W3 ~ PATHWAY1+NATORIDX.W1+ +EDYEARS.W1+HISPANIC.W1+SPANISH.W1+
                       FEMALE.W1+AGE.W1+INCOMER.W1,data=dtam2sub[! dtam2sub$PATHWAY3 %in% c(NA),])

lout1wave.dems <- lm(FTDEMS.W1 ~ PATHWAY1+NATORIDX.W1+ EDYEARS.W1+HISPANIC.W1+SPANISH.W1+
                       FEMALE.W1+AGE.W1+INCOMER.W1,data=dtam2sub[! dtam2sub$PATHWAY3 %in% c(NA),])
lout2wave.dems <- lm(FTDEMS.W2 ~ PATHWAY1+NATORIDX.W1+ EDYEARS.W1+HISPANIC.W1+SPANISH.W1+
                       FEMALE.W1+AGE.W1+INCOMER.W1,data=dtam2sub[! dtam2sub$PATHWAY3 %in% c(NA),])
lout3wave.dems <- lm(FTDEMS.W3 ~ PATHWAY1+NATORIDX.W1+ +EDYEARS.W1+HISPANIC.W1+SPANISH.W1+
                       FEMALE.W1+AGE.W1+INCOMER.W1,data=dtam2sub[! dtam2sub$PATHWAY3 %in% c(NA),])

### 
texreg(list(lout1wave.reps,
            lout2wave.reps,
            lout3wave.reps,
            lout1wave.dems,
            lout2wave.dems,
            lout3wave.dems),
       stars=0.05,digits=3,
       custom.model.names = c("4/16","10/16","10/18","4/16","10/16","10/18"))

